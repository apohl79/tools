// src/agent.ts
// SDK version: @anthropic-ai/claude-agent-sdk@0.2.0
// Verified types: query(), Options (systemPrompt, tools, canUseTool, includePartialMessages),
//   PermissionResult ({behavior:'allow',updatedInput}|{behavior:'deny',message}),
//   SDKMessage union (stream_event for deltas, assistant for final, result for terminal)
import type { ThreadMessage } from './types.ts';
import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { createInterface, type Interface as ReadlineInterface } from 'node:readline';
import { query, type Options } from '@anthropic-ai/claude-agent-sdk';

export interface AgentFactoryOptions {
  systemPreamble: string;
  tools: string[]; // allow-list
}

export type StreamChunk =
  | { type: 'delta'; text: string }
  | { type: 'done'; text: string }
  | { type: 'status'; status: string | null };

export interface ThreadAgent {
  send(userText: string): AsyncIterable<StreamChunk>;
  proposeConclusion(): AsyncIterable<StreamChunk>;
  snapshot(): ThreadMessage[];
  close?(): Promise<void>;
}

export type AgentFactory = (opts: AgentFactoryOptions) => ThreadAgent;

// Mock factory used by integration tests.
export function mockAgentFactory(script: {
  reply: string;
  conclusion: string;
}): AgentFactory {
  return (_opts) => {
    const messages: ThreadMessage[] = [];
    return {
      async *send(userText) {
        messages.push({ role: 'user', text: userText, ts: new Date().toISOString() });
        yield { type: 'delta', text: script.reply };
        messages.push({ role: 'assistant', text: script.reply, ts: new Date().toISOString() });
        yield { type: 'done', text: script.reply };
      },
      async *proposeConclusion() {
        yield { type: 'delta', text: script.conclusion };
        yield { type: 'done', text: script.conclusion };
      },
      snapshot: () => [...messages],
    };
  };
}

export interface CodexAgentFactoryConfig {
  command?: string;
  cwd?: string;
  args?: string[];
}

export function codexAgentFactory(config: CodexAgentFactoryConfig = {}): AgentFactory {
  return ({ systemPreamble }) => {
    const messages: ThreadMessage[] = [];
    const client = new CodexAppServerClient({
      command: config.command ?? 'codex',
      args: config.args ?? ['app-server'],
      cwd: config.cwd ?? process.cwd(),
      developerInstructions: buildCodexDeveloperInstructions(systemPreamble),
    });

    async function* runOnce(userText: string, kind: 'message' | 'conclusion'): AsyncIterable<StreamChunk> {
      const payload =
        kind === 'message'
          ? userText
          : 'Propose a 2-4 sentence conclusion of this thread. Be concrete. No preamble, no hedging.';
      let answer = '';
      for await (const chunk of client.runTurn(payload)) {
        if (chunk.type === 'delta') answer += chunk.text;
        else if (chunk.type === 'done') answer = chunk.text;
        yield chunk;
      }
      if (kind === 'message') {
        messages.push({ role: 'user', text: userText, ts: new Date().toISOString() });
        messages.push({ role: 'assistant', text: answer, ts: new Date().toISOString() });
      }
    }

    return {
      send: (t) => runOnce(t, 'message'),
      proposeConclusion: () => runOnce('', 'conclusion'),
      snapshot: () => [...messages],
      close: () => client.close(),
    } satisfies ThreadAgent;
  };
}

function buildCodexDeveloperInstructions(systemPreamble: string): string {
  return [
    'You are the assistant in one inline discussion thread.',
    'Answer only the current thread. Keep replies focused, concrete, and concise.',
    'Use read-only inspection when useful. Do not modify files.',
    '<system-preamble>',
    systemPreamble || '(none)',
    '</system-preamble>',
  ].join('\n');
}

interface CodexAppServerOptions {
  command: string;
  args: string[];
  cwd: string;
  developerInstructions: string;
}

interface JsonRpcResponse {
  id: number;
  result?: unknown;
  error?: { message?: string };
}

interface JsonRpcNotification {
  method: string;
  params?: unknown;
}

class CodexAppServerClient {
  private child: ChildProcessWithoutNullStreams | null = null;
  private rl: ReadlineInterface | null = null;
  private nextId = 1;
  private initialized: Promise<void> | null = null;
  private threadId: string | null = null;
  private stderr = '';
  private pending = new Map<number, {
    resolve: (value: unknown) => void;
    reject: (err: Error) => void;
  }>();
  private notifications: JsonRpcNotification[] = [];
  private notificationWaiters: Array<(value: JsonRpcNotification | null) => void> = [];

  constructor(private readonly opts: CodexAppServerOptions) {}

  async *runTurn(input: string): AsyncIterable<StreamChunk> {
    await this.ensureThread();
    const threadId = this.threadId!;
    this.notifications = [];
    await this.request('turn/start', {
      threadId,
      input: [{ type: 'text', text: input, text_elements: [] }],
      approvalPolicy: 'never',
      sandboxPolicy: { type: 'readOnly', networkAccess: true },
      cwd: this.opts.cwd,
    });

    yield* this.consumeTurn(threadId);
  }

  async close(): Promise<void> {
    this.resolveNotificationWaiters(null);
    for (const pending of this.pending.values()) {
      pending.reject(new Error('codex app-server closed'));
    }
    this.pending.clear();
    this.rl?.close();
    this.rl = null;
    if (this.child && !this.child.killed) {
      this.child.kill();
    }
    this.child = null;
  }

  private async ensureThread(): Promise<void> {
    if (!this.initialized) {
      this.initialized = this.initialize();
    }
    await this.initialized;
    if (this.threadId) return;

    const result = await this.request('thread/start', {
      cwd: this.opts.cwd,
      approvalPolicy: 'never',
      sandbox: 'read-only',
      developerInstructions: this.opts.developerInstructions,
      ephemeral: true,
    });
    const thread = isRecord(result) && isRecord(result['thread']) ? result['thread'] : null;
    const id = typeof thread?.['id'] === 'string' ? thread['id'] : null;
    if (!id) {
      throw new Error('codex app-server thread/start returned no thread id');
    }
    this.threadId = id;
  }

  private async initialize(): Promise<void> {
    this.start();
    await this.request('initialize', {
      clientInfo: {
        name: 'inline_discussion',
        title: 'Inline Discussion',
        version: '0.1.0',
      },
      capabilities: null,
    });
    this.notify('initialized', {});
  }

  private start(): void {
    if (this.child) return;
    const child = spawn(this.opts.command, this.opts.args, {
      cwd: this.opts.cwd,
      env: { ...process.env, CODEX_INLINE_DISCUSSION_CHILD: '1' },
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    this.child = child;
    this.rl = createInterface({ input: child.stdout, crlfDelay: Infinity });

    child.stderr.on('data', (chunk: Buffer) => {
      this.stderr += chunk.toString('utf8');
      if (this.stderr.length > 4000) this.stderr = this.stderr.slice(-4000);
    });
    child.on('error', (err) => this.failAll(err instanceof Error ? err : new Error(String(err))));
    child.on('close', (code, signal) => {
      const detail = signal ? `signal ${signal}` : `exit ${code ?? 1}`;
      const stderr = this.stderr.trim();
      this.failAll(new Error(`codex app-server closed with ${detail}${stderr ? `: ${truncateForError(stderr)}` : ''}`));
    });
    this.rl.on('line', (line) => this.handleLine(line));
  }

  private request(method: string, params: unknown): Promise<unknown> {
    this.start();
    const id = this.nextId++;
    this.write({ method, id, params });
    return new Promise((resolve, reject) => {
      this.pending.set(id, { resolve, reject });
    });
  }

  private notify(method: string, params: unknown): void {
    this.start();
    this.write({ method, params });
  }

  private write(message: unknown): void {
    if (!this.child) throw new Error('codex app-server not started');
    this.child.stdin.write(`${JSON.stringify(message)}\n`);
  }

  private handleLine(line: string): void {
    if (!line.trim()) return;
    let parsed: unknown;
    try {
      parsed = JSON.parse(line);
    } catch {
      return;
    }
    if (!isRecord(parsed)) return;
    if (typeof parsed['id'] === 'number') {
      this.handleResponse(parsed as unknown as JsonRpcResponse);
      return;
    }
    if (typeof parsed['method'] === 'string') {
      this.enqueueNotification(parsed as unknown as JsonRpcNotification);
    }
  }

  private handleResponse(response: JsonRpcResponse): void {
    const pending = this.pending.get(response.id);
    if (!pending) return;
    this.pending.delete(response.id);
    if (response.error) {
      pending.reject(new Error(response.error.message ?? 'codex app-server request failed'));
      return;
    }
    pending.resolve(response.result);
  }

  private enqueueNotification(notification: JsonRpcNotification): void {
    const waiter = this.notificationWaiters.shift();
    if (waiter) {
      waiter(notification);
      return;
    }
    this.notifications.push(notification);
  }

  private nextNotification(): Promise<JsonRpcNotification | null> {
    if (this.notifications.length > 0) {
      return Promise.resolve(this.notifications.shift()!);
    }
    return new Promise((resolve) => this.notificationWaiters.push(resolve));
  }

  private resolveNotificationWaiters(value: JsonRpcNotification | null): void {
    const waiters = this.notificationWaiters.splice(0);
    for (const waiter of waiters) waiter(value);
  }

  private failAll(err: Error): void {
    for (const pending of this.pending.values()) pending.reject(err);
    this.pending.clear();
    this.resolveNotificationWaiters(null);
  }

  private async *consumeTurn(threadId: string): AsyncIterable<StreamChunk> {
    let turnId: string | null = null;
    let finalText = '';
    let completedItemText = '';
    let statusActive = false;

    while (true) {
      const notification = await this.nextNotification();
      if (!notification) {
        throw new Error('codex app-server closed before turn completed');
      }
      const params = isRecord(notification.params) ? notification.params : {};
      const notificationThreadId = typeof params['threadId'] === 'string' ? params['threadId'] : null;
      if (notificationThreadId && notificationThreadId !== threadId) continue;

      if (notification.method === 'turn/started') {
        const turn = isRecord(params['turn']) ? params['turn'] : null;
        const id = typeof turn?.['id'] === 'string' ? turn['id'] : null;
        if (id) turnId = id;
        continue;
      }

      const notificationTurnId = typeof params['turnId'] === 'string' ? params['turnId'] : null;
      const turn = isRecord(params['turn']) ? params['turn'] : null;
      const completedTurnId = typeof turn?.['id'] === 'string' ? turn['id'] : null;
      const eventTurnId = notificationTurnId ?? completedTurnId;
      if (eventTurnId && turnId && eventTurnId !== turnId) continue;

      if (notification.method === 'item/started') {
        const item = isRecord(params['item']) ? params['item'] : params;
        const status = codexToolStatus(item);
        if (status) {
          statusActive = true;
          yield { type: 'status', status };
        }
      } else if (notification.method === 'item/agentMessage/delta' && typeof params['delta'] === 'string') {
        if (statusActive) {
          statusActive = false;
          yield { type: 'status', status: null };
        }
        finalText += params['delta'];
        yield { type: 'delta', text: params['delta'] };
      } else if (notification.method === 'item/completed') {
        const item = isRecord(params['item']) ? params['item'] : null;
        if (item?.['type'] === 'agentMessage' && typeof item['text'] === 'string') {
          completedItemText = item['text'];
        }
      } else if (notification.method === 'turn/completed') {
        const answer = finalText || completedItemText;
        yield { type: 'done', text: answer };
        return;
      } else if (notification.method === 'error') {
        const message = typeof params['message'] === 'string' ? params['message'] : 'codex app-server error';
        throw new Error(message);
      }
    }
  }
}

function isRecord(value: unknown): value is Record<string, unknown> {
  return typeof value === 'object' && value !== null;
}

function truncateForError(s: string): string {
  return s.length > 1200 ? `${s.slice(0, 1200)}...` : s;
}

// Tools permitted in inline-discussion threads. WebFetch excluded (SSRF risk).
const ALLOWED_TOOLS = ['Read', 'Grep', 'Glob', 'WebSearch'] as const;

export interface DispatchState {
  accumulated: string;
  status?: string | null;
}

export interface DispatchResult {
  chunks: StreamChunk[];
  endOfTurn: boolean;
}

// Pure mapper from an SDKMessage to StreamChunks and a turn-boundary flag.
// Exported for unit testing. Verified against SDK 0.2.0 SDKMessage union types.
//
// Why the turn-boundary must be `result`, not `assistant`:
// SDKAssistantMessage fires once per assistant turn, and a single user send
// can span multiple assistant turns when the model uses tools (text → tool_use
// → tool_result → more text). Emitting `done` on the first assistant message
// truncates replies at the first tool call. SDKResultMessage is the only
// SDK signal that marks the full user-turn boundary.
export function dispatchSdkMessage(evt: unknown, state: DispatchState): DispatchResult {
  const e = evt as Record<string, unknown>;
  const chunks: StreamChunk[] = [];

  if (e['type'] === 'stream_event') {
    // SDKPartialAssistantMessage.event is RawMessageStreamEvent (BetaRawMessageStreamEvent).
    const event = e['event'] as Record<string, unknown> | undefined;
    // When Claude pauses for a tool, one text content block ends and a new one
    // starts after the tool result. The SDK emits no whitespace delta across
    // that boundary, so naive concat gives "Foo.Bar." Insert a paragraph break
    // when a fresh text block starts on top of existing accumulated text.
    if (event?.['type'] === 'content_block_start') {
      const block = event['content_block'] as Record<string, unknown> | undefined;
      const status = claudeToolStatus(block);
      if (status) setDispatchStatus(state, chunks, status);
      if (block?.['type'] === 'text' && state.accumulated.length > 0 && !state.accumulated.endsWith('\n\n')) {
        const sep = '\n\n';
        state.accumulated += sep;
        setDispatchStatus(state, chunks, null);
        chunks.push({ type: 'delta', text: sep });
      }
    }
    if (event?.['type'] === 'content_block_delta') {
      const delta = event['delta'] as Record<string, unknown> | undefined;
      if (delta?.['type'] === 'text_delta' && typeof delta['text'] === 'string') {
        state.accumulated += delta['text'];
        setDispatchStatus(state, chunks, null);
        chunks.push({ type: 'delta', text: delta['text'] });
      }
    }
    return { chunks, endOfTurn: false };
  }

  if (e['type'] === 'result') {
    const subtype = e['subtype'];
    // Prefer state.accumulated: it carries the paragraph breaks we inject at
    // text-block boundaries (across tool pauses). The SDK's e['result'] string
    // re-flattens text blocks without those separators, so using it here drops
    // the visual break when `thread.message.done` replaces the streamed body.
    const finalText =
      state.accumulated.length > 0
        ? state.accumulated
        : subtype === 'success' && typeof e['result'] === 'string' && e['result']
          ? (e['result'] as string)
          : '';
    chunks.push({ type: 'done', text: finalText });
    state.accumulated = '';
    state.status = null;
    return { chunks, endOfTurn: true };
  }

  // SDKAssistantMessage and other intermediate messages are ignored —
  // text content already arrives via stream_event deltas.
  return { chunks, endOfTurn: false };
}

function setDispatchStatus(state: DispatchState, chunks: StreamChunk[], status: string | null): void {
  if ((state.status ?? null) === status) return;
  state.status = status;
  chunks.push({ type: 'status', status });
}

function claudeToolStatus(block: Record<string, unknown> | undefined): string | null {
  if (block?.['type'] !== 'tool_use') return null;
  const rawName = typeof block['name'] === 'string' ? block['name'] : '';
  if (!rawName.trim()) return null;
  return `Using ${formatToolName(rawName)}...`;
}

function codexToolStatus(item: Record<string, unknown>): string | null {
  const type = stringField(item, 'type');
  if (!type) return null;

  if (type === 'webSearch') return 'Searching the web...';
  if (type === 'imageView') return 'Inspecting image...';
  if (type === 'imageGeneration') return 'Generating image...';
  if (type === 'fileChange') return 'Reviewing file changes...';
  if (type === 'mcpToolCall') return `Using ${formatToolName(toolNameFrom(item) ?? 'MCP tool')}...`;

  const lower = type.toLowerCase();
  if (lower.includes('tool') || lower.includes('function')) {
    return `Using ${formatToolName(toolNameFrom(item) ?? type)}...`;
  }
  return null;
}

function toolNameFrom(item: Record<string, unknown>): string | null {
  const direct = firstString(
    item['toolName'],
    item['tool_name'],
    item['toolTitle'],
    item['tool_title'],
    item['name'],
    item['tool'],
    item['title'],
  );
  if (direct) return direct;

  const invocation = isRecord(item['invocation']) ? item['invocation'] : null;
  if (!invocation) return null;
  return firstString(
    invocation['toolName'],
    invocation['tool_name'],
    invocation['toolTitle'],
    invocation['tool_title'],
    invocation['name'],
    invocation['tool'],
    invocation['title'],
  );
}

function stringField(item: Record<string, unknown>, key: string): string | null {
  const value = item[key];
  return typeof value === 'string' && value.trim() ? value : null;
}

function firstString(...values: unknown[]): string | null {
  for (const value of values) {
    if (typeof value === 'string' && value.trim()) return value;
  }
  return null;
}

function formatToolName(raw: string): string {
  const trimmed = raw.trim();
  const withoutMcpPrefix = trimmed.startsWith('mcp__') ? (trimmed.split('__').pop() ?? trimmed) : trimmed;
  const spaced = withoutMcpPrefix.replace(/[_-]+/g, ' ').replace(/\s+/g, ' ').trim();
  return truncateStatusPart(spaced || 'tool');
}

function truncateStatusPart(value: string): string {
  return value.length > 80 ? `${value.slice(0, 77)}...` : value;
}

export function sdkAgentFactory(): AgentFactory {
  return ({ systemPreamble, tools }) => {
    const messages: ThreadMessage[] = [];
    const allowList = tools.filter((t) => (ALLOWED_TOOLS as readonly string[]).includes(t));

    // Push-based user-message queue drives multi-turn sessions via a single persistent query.
    let resolveNext: ((m: unknown) => void) | null = null;
    const queue: unknown[] = [];

    function push(msg: unknown): void {
      if (resolveNext) { resolveNext(msg); resolveNext = null; return; }
      queue.push(msg);
    }

    // Yields SDKUserMessage values; null sentinel ends the stream.
    // SDKUserMessage.session_id is assigned internally by the SDK.
    async function* userStream(): AsyncGenerator<unknown> {
      while (true) {
        if (queue.length > 0) {
          const msg = queue.shift();
          if (msg === null) return;
          yield msg;
          continue;
        }
        const next = await new Promise<unknown>((r) => { resolveNext = r; });
        if (next === null) return;
        yield next;
      }
    }

    const options: Options = {
      systemPrompt: systemPreamble || undefined,
      // Restrict available tools to the allow-list.
      tools: allowList,
      // Extra enforcement: deny anything outside the allow-list with a clear message.
      canUseTool: async (toolName, input, _opts) => {
        if ((ALLOWED_TOOLS as readonly string[]).includes(toolName)) {
          return { behavior: 'allow', updatedInput: input };
        }
        return {
          behavior: 'deny',
          message: `Tool '${toolName}' not allowed in inline-discussion. Allowed: ${allowList.join(', ')}.`,
        };
      },
      includePartialMessages: true,
    };

    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const sdkIter = query({ prompt: userStream() as any, options });
    let sdkDone = false;

    // Per-turn chunk queue shared between background sdkLoop and foreground runOnce.
    let chunkQueue: StreamChunk[] = [];
    let resolveChunk: ((c: StreamChunk | null) => void) | null = null;
    const dispatchState: DispatchState = { accumulated: '' };

    function emitChunk(c: StreamChunk): void {
      if (resolveChunk) { resolveChunk(c); resolveChunk = null; return; }
      chunkQueue.push(c);
    }

    function drainResolver(msg: StreamChunk | null): void {
      const r = resolveChunk;
      resolveChunk = null;
      if (r) r(msg);
    }

    function dispatchEvent(evt: unknown): void {
      const { chunks, endOfTurn } = dispatchSdkMessage(evt, dispatchState);
      for (const c of chunks) emitChunk(c);
      if (endOfTurn) drainResolver(null);
    }

    const sdkLoop = (async () => {
      try {
        for await (const evt of sdkIter) {
          dispatchEvent(evt);
        }
      } finally {
        sdkDone = true;
        drainResolver(null);
      }
    })();

    // Serialises concurrent runOnce calls so shared chunkQueue/resolveChunk state
    // is never reset while an earlier turn is still reading from it.
    let runLock: Promise<void> = Promise.resolve();

    async function* runOnce(userText: string, kind: 'message' | 'conclusion'): AsyncIterable<StreamChunk> {
      const prevLock = runLock;
      let releaseLock: () => void = () => {};
      runLock = new Promise<void>((r) => { releaseLock = r; });
      await prevLock;

      try {
        const payload =
          kind === 'message'
            ? userText
            : 'Propose a 2-4 sentence conclusion of this thread. Be concrete. No preamble, no hedging.';

        chunkQueue = [];
        resolveChunk = null;
        // Push SDKUserMessage; session_id left empty — SDK assigns it internally.
        push({ type: 'user', message: { role: 'user', content: payload }, parent_tool_use_id: null, session_id: '' });

        while (true) {
          if (chunkQueue.length > 0) {
            const c = chunkQueue.shift()!;
            yield c;
            if (c.type === 'done') {
              if (kind === 'message') {
                messages.push({ role: 'user', text: userText, ts: new Date().toISOString() });
                messages.push({ role: 'assistant', text: c.text, ts: new Date().toISOString() });
              }
              return;
            }
            continue;
          }
          const c = await new Promise<StreamChunk | null>((r) => { resolveChunk = r; });
          if (!c) return;
          yield c;
          if (c.type === 'done') {
            if (kind === 'message') {
              messages.push({ role: 'user', text: userText, ts: new Date().toISOString() });
              messages.push({ role: 'assistant', text: c.text, ts: new Date().toISOString() });
            }
            return;
          }
        }
      } finally {
        releaseLock();
      }
    }

    return {
      send: (t) => runOnce(t, 'message'),
      proposeConclusion: () => runOnce('', 'conclusion'),
      snapshot: () => [...messages],
      close: async () => {
        push(null);
        if (!sdkDone) await sdkLoop;
      },
    } satisfies ThreadAgent;
  };
}
