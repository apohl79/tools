import { readFileSync, writeFileSync, renameSync } from 'node:fs';
import { marked } from 'marked';
import { normalizeDetailsSpacing, parseDoc } from './markdown.ts';
import { escapeHtml } from './html.ts';
import { findArchivedBlocks } from './archive.ts';
import type { ThreadKind, ThreadMessage } from './types.ts';

export interface AppendThreadInput {
  kind?: ThreadKind;
  blockId: string;
  quote?: string;
  transcript: ThreadMessage[];
  conclusion: string;
  date: string; // ISO 8601 date, e.g. '2026-04-19'
  // Optional stable id for the resulting <details> block. Written as
  // `data-thread-id="<id>"` so later operations (e.g. edit conclusion) can
  // locate the specific block unambiguously. Omitted blocks stay read-only.
  threadId?: string;
}

export function appendThreadDetails(docPath: string, input: AppendThreadInput): void {
  const original = readFileSync(docPath, 'utf8');
  const updated = rewriteDoc(original, input);
  const tmp = `${docPath}.tmp`;
  writeFileSync(tmp, updated);
  renameSync(tmp, docPath);
}

/**
 * Remove the N-th archived `<details>` block from the doc (1-indexed, matching
 * `parseArchivedThreads`'s `archived-N` counter). Covers both the new
 * self-contained format and the legacy split format. The separator newline
 * that immediately follows the block is consumed too so we don't leave a
 * stray blank line.
 */
export function removeArchivedBlockByIndex(docPath: string, archiveIndex: number): void {
  const source = readFileSync(docPath, 'utf8');
  const updated = removeArchivedBlock(source, archiveIndex);
  const tmp = `${docPath}.tmp`;
  writeFileSync(tmp, updated);
  renameSync(tmp, docPath);
}

export function removeArchivedBlock(source: string, archiveIndex: number): string {
  // Mirror rewriteDoc: parseDoc and findArchivedBlocks normalize `</details>\n`
  // → `</details>\n\n` before lexing, so the block.markdown tokens we look up
  // via indexOf must be searched (and sliced) in the same normalized text.
  const normalized = normalizeDetailsSpacing(source);
  const range = findArchivedBlockRange(normalized, archiveIndex);
  if (!range) throw new Error(`archived thread #${archiveIndex} not found`);
  // Eat one trailing blank-line (the separator we wrote when inserting).
  let end = range.end;
  if (normalized[end] === '\n') end += 1;
  if (normalized[end] === '\n') end += 1;
  return normalized.slice(0, range.start) + normalized.slice(end);
}

function findArchivedBlockRange(source: string, archiveIndex: number): { start: number; end: number } | null {
  // Use `findArchivedBlocks` as the single source of truth for which blocks
  // count as archives. A previous version duplicated the identification logic
  // here, and the two counters drifted whenever a `<details>` block matched
  // the summary pattern but failed a deeper check in `parseArchivedThreads`
  // (missing anchor, missing required child divs, etc.). With the shared
  // helper the N-th entry here always refers to the same block as
  // `archived-N` in the client's state.
  const blocks = parseDoc(source).blocks;
  const archives = findArchivedBlocks(source);
  const target = archives[archiveIndex - 1];
  if (!target) return null;
  const targetBlock = blocks[target.blockIndex]!;

  // Walk through blocks in order to find the exact source position of the
  // target. `indexOf` from 0 would return the first occurrence of identical
  // raw text, which is wrong when an earlier block happens to have the same
  // markdown (rare but possible).
  let cursor = 0;
  for (let i = 0; i <= target.blockIndex; i += 1) {
    const b = blocks[i]!;
    const idx = source.indexOf(b.markdown, cursor);
    if (idx === -1) return null;
    if (i === target.blockIndex) {
      // Self-contained block closes at the trailing </details>.
      if (/<\/details>\s*$/i.test(targetBlock.markdown)) {
        return { start: idx, end: idx + targetBlock.markdown.length };
      }
      // Legacy split format: first `</details>` after the opener closes the block.
      const close = source.indexOf('</details>', idx + targetBlock.markdown.length);
      if (close === -1) return null;
      return { start: idx, end: close + '</details>'.length };
    }
    cursor = idx + b.markdown.length;
  }
  return null;
}

/**
 * Rewrite an existing archived `<details data-thread-id="…">` block with a
 * fresh conclusion. The entire block is regenerated from `input` (which must
 * include the same `threadId`), so the transcript and messages are preserved
 * and only the conclusion div changes.
 *
 * Throws if no `<details>` with the given `data-thread-id` is found — i.e. the
 * archive was written by an older version without the id, or the user is
 * trying to edit a pre-archived (loaded-from-disk) thread.
 */
export function replaceThreadDetails(docPath: string, input: AppendThreadInput): void {
  if (!input.threadId) throw new Error('replaceThreadDetails requires threadId');
  const original = readFileSync(docPath, 'utf8');
  const { start, end } = findDetailsBlock(original, input.threadId);
  const details = formatDetails(input);
  const before = original.slice(0, start);
  const after = original.slice(end);
  const updated = `${before}${details}${after}`;
  const tmp = `${docPath}.tmp`;
  writeFileSync(tmp, updated);
  renameSync(tmp, docPath);
}

/**
 * Locate the character range of a `<details data-thread-id="…">…</details>`
 * block. Assumes the block is emitted as a single HTML block without nested
 * `<details>` (enforced by `formatDetails`), so the first `</details>` after
 * the opener closes the block.
 */
function findDetailsBlock(source: string, threadId: string): { start: number; end: number } {
  // `formatDetails` emits openers in a single fixed shape
  // `<details data-thread-id="...">`, so a literal search matches unambiguously
  // and avoids the ReDoS risk of a dynamic RegExp.
  const opener = `<details data-thread-id="${escapeHtml(threadId)}">`;
  const start = source.indexOf(opener);
  if (start === -1) throw new Error(`<details> with data-thread-id="${threadId}" not found in doc`);
  const closeIdx = source.indexOf('</details>', start + opener.length);
  if (closeIdx === -1) throw new Error(`unterminated <details> for thread-id="${threadId}"`);
  return { start, end: closeIdx + '</details>'.length };
}

export function rewriteDoc(original: string, input: AppendThreadInput): string {
  // Guarantee a blank line after every `</details>` in the source. CommonMark
  // HTML block type 6 only ends at a blank line — without one, the paragraph
  // that follows is absorbed into the HTML block and its inline markdown
  // (`**bold**`, `*italic*`) is rendered literally. Applying the fix here
  // also repairs docs that were written by earlier versions of this function.
  const source = normalizeDetailsSpacing(original);
  const parsed = parseDoc(source);
  const target = parsed.blocks.find((b) => b.id === input.blockId);
  if (!target) throw new Error(`blockId not found: ${input.blockId}`);

  // Locate the target block's raw text in the source — scan from the start,
  // skipping earlier occurrences of identical raw text by consuming blocks in order.
  let cursor = 0;
  for (const b of parsed.blocks) {
    const idx = source.indexOf(b.markdown, cursor);
    if (idx === -1) throw new Error(`block raw not found in source: ${b.id}`);
    if (b.id === input.blockId) {
      const insertAt = idx + b.markdown.length;
      const before = source.slice(0, insertAt);
      const after = source.slice(insertAt);
      const details = formatDetails(input);
      const needsLeadingNewlines = before.endsWith('\n\n') ? '' : before.endsWith('\n') ? '\n' : '\n\n';
      // Same reason as the normalize step above: ensure a blank line after
      // `</details>` so the next paragraph parses as markdown, not as a
      // continuation of the HTML block.
      const needsTrailingNewlines = after.startsWith('\n\n') ? '' : after.startsWith('\n') ? '\n' : '\n\n';
      return `${before}${needsLeadingNewlines}${details}${needsTrailingNewlines}${after}`;
    }
    cursor = idx + b.markdown.length;
  }
  // Unreachable if findIndex returned a valid index.
  throw new Error(`blockId not found while scanning: ${input.blockId}`);
}

/**
 * Emits the archived thread as a SINGLE CommonMark HTML block — no blank
 * lines between `<details>` and `</details>`. This is load-bearing:
 *
 *  - CommonMark HTML block type 6 ends at the first blank line, so any blank
 *    line between the `<details>` opener and its content splits the archive
 *    across multiple marked blocks. Each block is then DOMPurify-sanitised
 *    independently, which auto-closes the lone `<details><summary>…</summary>`
 *    chunk and leaves the transcript as DOM siblings outside the details.
 *  - Instead, we pre-render each message body as inline-only HTML (via
 *    `marked.parseInline` after HTML-escaping), wrap it in a `<div>` carrying
 *    the original markdown in a `data-raw` attribute, and keep every logical
 *    line non-empty so the HTML block stays contiguous.
 *  - The archive parser (`parseArchivedThreads`) reads `data-raw` to restore
 *    the messages verbatim on resume — no markdown→HTML→markdown round-trip
 *    loss.
 */
function formatDetails(input: AppendThreadInput): string {
  const safeQuote = input.quote ? escapeHtml(input.quote) : null;
  const label = safeQuote ? `"${safeQuote}"` : 'entire block';
  const date = escapeHtml(input.date);
  const idAttr = input.threadId ? ` data-thread-id="${escapeHtml(input.threadId)}"` : '';
  const lines: string[] = [];

  if (input.kind === 'note') {
    lines.push(`<details${idAttr}><summary>📝 Note on ${label} — ${date}</summary>`);
    lines.push(
      `<div class="archived-note" data-kind="note" data-raw="${encodeAttr(input.conclusion)}">` +
      `${renderBody(input.conclusion)}</div>`,
    );
    lines.push('</details>');
    return lines.join('\n');
  }

  lines.push(`<details${idAttr}><summary>💬 Thread on ${label} — ${date}</summary>`);
  for (const m of input.transcript) {
    const roleLabel = m.role === 'user' ? 'User' : 'Claude';
    lines.push(
      `<div class="archived-msg" data-role="${m.role}" data-raw="${encodeAttr(m.text)}">` +
      `<strong>${roleLabel}:</strong> ${renderBody(m.text)}</div>`,
    );
  }
  lines.push(
    `<div class="archived-conclusion" data-raw="${encodeAttr(input.conclusion)}">` +
    `<strong>Conclusion:</strong> ${renderBody(input.conclusion)}</div>`,
  );
  lines.push('</details>');
  return lines.join('\n');
}

/**
 * Render a message body as HTML. HTML-escape first so a user message
 * containing `</details>` can't break out, then let marked turn the
 * remaining markdown into full block HTML (headings, lists, blockquotes,
 * code, plus inline formatting). Blank lines inside the output would end
 * the surrounding CommonMark HTML block and let DOMPurify auto-close the
 * `<details>` opener, so we collapse them to single newlines — except
 * inside `<pre>` where blank lines are meaningful code content and are
 * instead encoded as `&#10;` entities so the containing HTML block stays
 * contiguous without losing rendered blank lines in code.
 */
function renderBody(text: string): string {
  if (!text) return '';
  // escapeHtml turns `<` → `&lt;`; marked leaves existing entities alone, so
  // the literal tag stays visible as text. `&amp;` likewise round-trips.
  const escaped = escapeHtml(text);
  const rendered = marked.parse(escaped, { breaks: true, gfm: true, async: false }) as string;
  const preProtected = rendered.replace(/<pre[\s\S]*?<\/pre>/gi, (block) =>
    block.replace(/\n/g, '&#10;'),
  );
  const collapsed = preProtected.replace(/\n\s*\n/g, '\n').trimEnd();
  // If the reply is just one paragraph, unwrap `<p>…</p>` so short text
  // flows inline next to the `<strong>Claude:</strong>` prefix instead of
  // dropping to a new line. Any sign of another block tag inside the
  // captured content (second `<p>`, heading, list, pre, blockquote, table)
  // keeps the block structure.
  const m = collapsed.match(/^<p>([\s\S]*)<\/p>$/i);
  const inner = m?.[1] ?? '';
  if (m && !/<(?:p|h[1-6]|ul|ol|pre|blockquote|table)[\s>]/i.test(inner)) {
    return inner;
  }
  return collapsed;
}

/**
 * HTML-attribute-safe encoding. `data-raw` round-trips the original markdown
 * back to the archive parser; the parser uses `getAttribute('data-raw')`
 * which auto-decodes entities including `&#10;` and `&#13;`.
 */
function encodeAttr(s: string): string {
  return s
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;')
    .replace(/\r/g, '&#13;')
    .replace(/\n/g, '&#10;');
}
