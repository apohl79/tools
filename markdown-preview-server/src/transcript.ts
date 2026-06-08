// src/transcript.ts
import { readFileSync } from 'node:fs';

export interface JsonlEntry {
  type: 'user' | 'assistant' | 'tool_use' | 'tool_result';
  text?: string;
  name?: string;
  args?: string;
  kind?: string;
  [key: string]: unknown;
}

export function readJsonl(path: string): JsonlEntry[] {
  const raw = readFileSync(path, 'utf8');
  return raw
    .split('\n')
    .map((line) => line.trim())
    .filter((line) => line.length > 0)
    .flatMap((line) => normalizeJsonlEntry(JSON.parse(line) as unknown));
}

const ELIDE_LIMIT = 4 * 1024;
const TOTAL_LIMIT = 80 * 1024;

export interface TrimOptions {
  elideBytes?: number;
  maxBytes?: number;
}

export function trimTranscript(entries: JsonlEntry[], opts: TrimOptions = {}): string {
  const elideBytes = opts.elideBytes ?? ELIDE_LIMIT;
  const maxBytes = opts.maxBytes ?? TOTAL_LIMIT;

  const firstUserIdx = entries.findIndex((e) => e.type === 'user');
  const lastAssistantIdx = findLastIndex(entries, (e) => e.type === 'assistant');

  const rendered: string[] = entries.map((entry, idx) => renderEntry(entry, idx, elideBytes));

  let total = rendered.reduce((n, s) => n + Buffer.byteLength(s), 0);
  if (total <= maxBytes) return rendered.join('\n\n');

  // Drop tool results first (oldest), then oldest text messages.
  const drop = new Set<number>();
  const dropCandidates = entries
    .map((e, i) => ({ e, i }))
    .filter(({ e, i }) => e.type === 'tool_result' && i !== firstUserIdx && i !== lastAssistantIdx);
  for (const c of dropCandidates) {
    if (total <= maxBytes) break;
    drop.add(c.i);
    total -= Buffer.byteLength(rendered[c.i]!);
  }
  if (total > maxBytes) {
    const textCandidates = entries
      .map((e, i) => ({ e, i }))
      .filter(({ e, i }) =>
        (e.type === 'user' || e.type === 'assistant') &&
        i !== firstUserIdx &&
        i !== lastAssistantIdx,
      );
    for (const c of textCandidates) {
      if (total <= maxBytes) break;
      drop.add(c.i);
      total -= Buffer.byteLength(rendered[c.i]!);
    }
  }
  return rendered.filter((_, i) => !drop.has(i)).join('\n\n');
}

function renderEntry(entry: JsonlEntry, _i: number, elideBytes: number): string {
  switch (entry.type) {
    case 'user':
      return `USER: ${redactSecrets(entry.text ?? '')}`;
    case 'assistant':
      return `ASSISTANT: ${redactSecrets(entry.text ?? '')}`;
    case 'tool_use':
      return `TOOL ${entry.name}(${truncate(redactSecrets(entry.args ?? ''), 200)})`;
    case 'tool_result': {
      const kind = entry.kind ?? 'unknown';
      const raw = entry.text ?? '';
      if (Buffer.byteLength(raw) > elideBytes) {
        const preview = redactSecrets(raw.slice(0, 200));
        return `[tool result elided — ${kind}: ${preview}…]`;
      }
      return `TOOL_RESULT ${kind}: ${redactSecrets(raw)}`;
    }
    default:
      return '';
  }
}

function normalizeJsonlEntry(raw: unknown): JsonlEntry[] {
  if (!isRecord(raw)) return [];

  if (isLegacyTranscriptEntry(raw)) {
    return [raw];
  }

  if (raw['type'] !== 'response_item') return [];
  const payload = raw['payload'];
  if (!isRecord(payload)) return [];

  if (payload['type'] === 'message') {
    const role = payload['role'];
    if (role !== 'user' && role !== 'assistant') return [];
    const text = extractContentText(payload['content']);
    return text ? [{ type: role, text }] : [];
  }

  if (payload['type'] === 'function_call') {
    const name = typeof payload['name'] === 'string' ? payload['name'] : 'unknown';
    const args = typeof payload['arguments'] === 'string'
      ? payload['arguments']
      : JSON.stringify(payload['arguments'] ?? {});
    return [{ type: 'tool_use', name, args }];
  }

  if (payload['type'] === 'function_call_output') {
    const output = typeof payload['output'] === 'string'
      ? payload['output']
      : JSON.stringify(payload['output'] ?? '');
    return [{ type: 'tool_result', kind: 'codex', text: output }];
  }

  return [];
}

function isLegacyTranscriptEntry(raw: Record<string, unknown>): raw is JsonlEntry {
  const type = raw['type'];
  return (
    (type === 'user' || type === 'assistant' || type === 'tool_use' || type === 'tool_result') &&
    ['text', 'name', 'args', 'kind'].some((key) => raw[key] !== undefined)
  );
}

function extractContentText(content: unknown): string {
  if (typeof content === 'string') return content;
  if (!Array.isArray(content)) return '';
  const parts: string[] = [];
  for (const item of content) {
    if (typeof item === 'string') {
      parts.push(item);
      continue;
    }
    if (!isRecord(item)) continue;
    const text = item['text'];
    if (typeof text === 'string') {
      parts.push(text);
    } else if (item['type'] === 'input_image') {
      parts.push('[image]');
    }
  }
  return parts.join('\n');
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

function truncate(s: string, n: number): string {
  return s.length > n ? `${s.slice(0, n)}…` : s;
}

// F21 / spec §10.2 — replace common secret patterns before any other processing.
const SECRET_PATTERNS: Array<{ name: string; re: RegExp }> = [
  { name: 'anthropic-or-openai-key', re: /\bsk-[A-Za-z0-9_-]{20,}\b/g },
  { name: 'aws-access-key',           re: /\bA(KIA|SIA)[0-9A-Z]{16}\b/g },
  { name: 'jwt',                      re: /\beyJ[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\.[A-Za-z0-9_-]+\b/g },
  { name: 'pem-block',                re: /-----BEGIN [A-Z ]+-----[\s\S]+?-----END [A-Z ]+-----/g },
  { name: 'github-token',             re: /\bgh[pousr]_[A-Za-z0-9]{20,}\b/g },
  { name: 'slack-token',              re: /\bxox[baprs]-[A-Za-z0-9-]+/g },
  { name: 'bearer',                   re: /\bBearer [A-Za-z0-9_.+/=-]{20,}\b/g },
  { name: 'password-assignment',      re: /(password|passwd|secret|api[_-]?key|token)\s*[:=]\s*['"]?([^\s'"]{6,})['"]?/gi },
  { name: 'high-entropy-b64',         re: /\b[A-Za-z0-9+/]{40,}={0,2}\b/g },
];

export function redactSecrets(input: string): string {
  let out = input;
  for (const { name, re } of SECRET_PATTERNS) {
    out = out.replace(re, `[secret redacted: ${name}]`);
  }
  return out;
}

function findLastIndex<T>(arr: T[], pred: (t: T) => boolean): number {
  for (let i = arr.length - 1; i >= 0; i -= 1) {
    if (pred(arr[i]!)) return i;
  }
  return -1;
}
