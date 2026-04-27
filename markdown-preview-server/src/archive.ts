// src/archive.ts
import { JSDOM } from 'jsdom';
import { parseDoc } from './markdown.ts';
import type { Anchor, Thread, ThreadKind, ThreadMessage, Block } from './types.ts';

const THREAD_HEADER_RE =
  /💬\s+Thread on (?:"(?<quote>[^"]*)"|entire block)\s+—\s+(?<date>\d{4}-\d{2}-\d{2})/;
const NOTE_HEADER_RE =
  /📝\s+Note on (?:"(?<quote>[^"]*)"|entire block)\s+—\s+(?<date>\d{4}-\d{2}-\d{2})/;

export function parseArchivedThreads(markdown: string): Thread[] {
  return findArchivedBlocks(markdown).map((b) => b.thread);
}

/**
 * Archive block info — pairs a parsed `Thread` with the index of its opening
 * `<details>` block in `parseDoc(markdown).blocks`. Callers that need to
 * locate the block's source range (e.g. to remove it) use this to avoid
 * re-implementing the archive identification logic and risking a counter
 * drift from `parseArchivedThreads`.
 */
export interface ArchivedBlockInfo {
  thread: Thread;
  blockIndex: number;
}

/**
 * Single source of truth for archive block identification. Every filter
 * condition applied here must also gate the `counter` increment, so the
 * `archived-N` ids emitted in `thread.id` stay aligned with the order of
 * returned `blockIndex` values. `parseArchivedThreads` is a thin wrapper.
 */
export function findArchivedBlocks(markdown: string): ArchivedBlockInfo[] {
  const blocks = parseDoc(markdown).blocks;
  const out: ArchivedBlockInfo[] = [];
  let counter = 0;

  for (let i = 0; i < blocks.length; i += 1) {
    const block = blocks[i]!;
    if (block.kind !== 'html') continue;

    // `<details>` openers may carry attributes (e.g. `data-thread-id`) written
    // by newer versions of this code — accept them all, only the summary text
    // matters for kind/date extraction.
    const openMatch = block.markdown.match(/^<details[^>]*><summary>([^<]+)<\/summary>/);
    if (!openMatch) continue;

    const summary = openMatch[1]!;
    const kind: ThreadKind | null = THREAD_HEADER_RE.test(summary)
      ? 'thread'
      : NOTE_HEADER_RE.test(summary)
        ? 'note'
        : null;
    if (kind === null) continue;
    const headerMatch = summary.match(kind === 'thread' ? THREAD_HEADER_RE : NOTE_HEADER_RE)!;
    const ts = `${headerMatch.groups!['date']}T00:00:00Z`;

    const anchorBlock = findPreviousContentBlock(blocks, i);
    if (!anchorBlock) continue;

    const rawQuote = headerMatch.groups!['quote'];
    const quote = rawQuote !== undefined ? decodeHtml(rawQuote) : undefined;
    const anchor: Anchor = quote
      ? { blockId: anchorBlock.id, quote, occurrence: 1 }
      : { blockId: anchorBlock.id };

    // Detect whether this archive uses the new self-contained format (the
    // whole <details>…</details> lives in this one html block) or the legacy
    // multi-block format (content split across paragraph blocks, closing
    // </details> as a separate html block).
    const selfContained = /<\/details>\s*$/i.test(block.markdown);

    const thread = selfContained
      ? parseSelfContainedArchive(block.markdown, kind, ts, anchor, counter + 1)
      : parseLegacyArchive(blocks, i, kind, ts, anchor, counter + 1);

    if (!thread) continue;
    counter += 1;
    out.push({ thread, blockIndex: i });
  }
  return out;
}

/**
 * Parse the new single-HTML-block archive format, where messages live inside
 * `<div class="archived-msg" data-role="…" data-raw="…">` and the conclusion
 * inside `<div class="archived-conclusion" data-raw="…">` (notes use
 * `<div class="archived-note" data-raw="…">`). `data-raw` carries the
 * original markdown so we round-trip verbatim.
 */
function parseSelfContainedArchive(
  markdown: string,
  kind: ThreadKind,
  ts: string,
  anchor: Anchor,
  counter: number,
): Thread | null {
  const dom = new JSDOM(markdown);
  const details = dom.window.document.querySelector('details');
  if (!details) return null;

  if (kind === 'note') {
    const noteDiv = details.querySelector('.archived-note');
    const noteText = noteDiv?.getAttribute('data-raw') ?? '';
    if (!noteText) return null;
    return {
      id: `archived-${counter}`,
      kind: 'note',
      anchor,
      status: 'archived',
      messages: [{ role: 'user', text: noteText, ts }],
      conclusion: noteText,
      createdAt: ts,
      closedAt: ts,
      closedBy: 'user',
    };
  }

  const messages: ThreadMessage[] = [];
  for (const msgDiv of details.querySelectorAll('.archived-msg')) {
    const role = msgDiv.getAttribute('data-role') === 'assistant' ? 'assistant' : 'user';
    const text = msgDiv.getAttribute('data-raw') ?? '';
    messages.push({ role, text, ts });
  }
  if (messages.length === 0) return null;

  const conclDiv = details.querySelector('.archived-conclusion');
  const conclusion = conclDiv?.getAttribute('data-raw') ?? '';
  return {
    id: `archived-${counter}`,
    kind: 'thread',
    anchor,
    status: 'archived',
    messages,
    conclusion,
    createdAt: ts,
    closedAt: ts,
    closedBy: 'user',
  };
}

/**
 * Legacy parser retained for docs written before the single-block rewrite.
 * Scans paragraph blocks following an opening `<details><summary>…</summary>`
 * html block and stops at the first `</details>` html block.
 */
function parseLegacyArchive(
  blocks: Block[],
  i: number,
  kind: ThreadKind,
  ts: string,
  anchor: Anchor,
  counter: number,
): Thread | null {
  const messages: ThreadMessage[] = [];
  let conclusionText: string | null = null;
  const noteParagraphs: string[] = [];
  let j = i + 1;

  while (j < blocks.length) {
    const b = blocks[j]!;
    if (b.kind === 'html' && b.markdown.trim().startsWith('</details>')) break;
    if (b.kind === 'paragraph') {
      const m = b.markdown.trim();
      if (kind === 'thread') {
        if (m.startsWith('**User:**')) {
          messages.push({ role: 'user', text: decodeHtml(m.slice('**User:**'.length).trim()), ts });
        } else if (m.startsWith('**Claude:**')) {
          messages.push({ role: 'assistant', text: decodeHtml(m.slice('**Claude:**'.length).trim()), ts });
        } else if (m.startsWith('**Conclusion:**')) {
          conclusionText = m.slice('**Conclusion:**'.length).trim();
        }
      } else {
        noteParagraphs.push(m);
      }
    }
    j += 1;
  }

  if (kind === 'thread' && (messages.length === 0 || !conclusionText)) return null;
  if (kind === 'note' && noteParagraphs.length === 0) return null;

  if (kind === 'note') {
    const noteText = decodeHtml(noteParagraphs.join('\n\n'));
    return {
      id: `archived-${counter}`,
      kind: 'note',
      anchor,
      status: 'archived',
      messages: [{ role: 'user', text: noteText, ts }],
      conclusion: noteText,
      createdAt: ts,
      closedAt: ts,
      closedBy: 'user',
    };
  }
  return {
    id: `archived-${counter}`,
    kind: 'thread',
    anchor,
    status: 'archived',
    messages,
    conclusion: decodeHtml(conclusionText!),
    createdAt: ts,
    closedAt: ts,
    closedBy: 'user',
  };
}

function findPreviousContentBlock(blocks: Block[], i: number): Block | undefined {
  for (let j = i - 1; j >= 0; j -= 1) {
    if (blocks[j]!.kind !== 'html') return blocks[j]!;
  }
  return undefined;
}

function decodeHtml(s: string): string {
  return s
    .replace(/<br\s*\/?>/gi, '\n')
    .replace(/&lt;/g, '<')
    .replace(/&gt;/g, '>')
    .replace(/&quot;/g, '"')
    .replace(/&amp;/g, '&');
}
