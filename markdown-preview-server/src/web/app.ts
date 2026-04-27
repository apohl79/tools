// src/web/app.ts
import DOMPurify from 'dompurify';
import { Marked } from 'marked';
import hljs from 'highlight.js/lib/common';
import type { Thread } from '../types.ts';

// Dedicated marked instance for rendering thread messages. GFM on so tables +
// fenced code work. `breaks: true` so assistant single-newlines survive as
// <br> inside paragraphs (matches how Claude's replies flow).
const msgMarked = new Marked({
  gfm: true,
  breaks: true,
  renderer: {
    code(code: string, infostring?: string): string {
      const lang = (infostring ?? '').trim().split(/\s+/)[0] || undefined;
      const language = lang && hljs.getLanguage(lang) ? lang : undefined;
      const highlighted = language
        ? hljs.highlight(code, { language }).value
        : hljs.highlightAuto(code).value;
      const cls = language ? `hljs language-${language}` : 'hljs';
      return `<pre><code class="${cls}">${highlighted}</code></pre>\n`;
    },
  },
});

function renderMarkdown(text: string): string {
  const raw = msgMarked.parse(text) as string;
  return DOMPurify.sanitize(raw, {
    ADD_TAGS: ['details', 'summary'] as string[],
  });
}

interface Bootstrap {
  html: string;
  blockIds: string[];
  title: string;
  threads: Thread[];
  archivedThreads: Thread[];
}

interface Prefs {
  theme?: 'light' | 'dark' | 'auto';
  width?: 'comfortable' | 'full';
}

const state = {
  threads: new Map<string, Thread>(),
  archived: new Map<string, Thread>(),
  activeThreadId: null as string | null,
  prefs: { theme: 'auto', width: 'comfortable' } as Required<Prefs>,
};

init().catch((err) => console.error(err));

async function init(): Promise<void> {
  await loadAndApplyPrefs();
  document.getElementById('theme-toggle')!.addEventListener('click', toggleTheme);
  document.getElementById('width-toggle')!.addEventListener('click', toggleWidth);
  document.getElementById('finish-top')!.addEventListener('click', finish);
  document.getElementById('finish-bottom')!.addEventListener('click', finish);

  const boot = (await (await fetch('/api/bootstrap')).json()) as Bootstrap;
  applyTitle(boot.title);
  renderDoc(boot.html);
  installBlockPluses();
  installThreadQuoteSelection();
  for (const t of boot.threads) state.threads.set(t.id, t);
  for (const t of boot.archivedThreads) state.archived.set(t.id, t);
  renderExistingThreads();

  const es = new EventSource('/events');
  es.addEventListener('thread.message.delta', (e) => onDelta(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.message.done', (e) => onDone(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.message.error', (e) => onMessageError(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.conclusion.proposed', (e) => onConclusion(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.closed', (e) => onClosed(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.deleted', (e) => onDeleted(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('thread.updated', (e) => onUpdated(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('doc.updated', (e) => onDocUpdated(JSON.parse((e as MessageEvent).data)));
  es.addEventListener('server.finished', (e) => onFinished(JSON.parse((e as MessageEvent).data)));
}

async function loadAndApplyPrefs(): Promise<void> {
  try {
    const r = await fetch('/api/prefs');
    if (r.ok) {
      const p = (await r.json()) as Prefs;
      if (p.theme) state.prefs.theme = p.theme;
      if (p.width) state.prefs.width = p.width;
    }
  } catch {
    // server-side prefs unavailable — keep defaults
  }
  applyTheme();
  applyWidth();
}

function applyTheme(): void {
  const t = state.prefs.theme;
  document.documentElement.dataset.theme = t === 'auto'
    ? (matchMedia('(prefers-color-scheme: dark)').matches ? 'dark' : 'light')
    : t;
  const btn = document.getElementById('theme-toggle')!;
  btn.textContent = document.documentElement.dataset.theme === 'dark' ? '☀' : '☾';
  btn.setAttribute('aria-label', `Theme: ${t}`);
}

function applyWidth(): void {
  const w = state.prefs.width;
  document.documentElement.dataset.width = w;
  const btn = document.getElementById('width-toggle')!;
  btn.innerHTML = w === 'full'
    ? '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="M2 5h16M2 10h16M2 15h16" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round"/></svg><span>Full</span>'
    : '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="M5 5h10M5 10h10M5 15h10" stroke="currentColor" stroke-width="1.5" fill="none" stroke-linecap="round"/></svg><span>Comfortable</span>';
}

async function savePrefs(patch: Prefs): Promise<void> {
  Object.assign(state.prefs, patch);
  try {
    await fetch('/api/prefs', {
      method: 'POST',
      headers: { 'content-type': 'application/json' },
      body: JSON.stringify(patch),
    });
  } catch (err) {
    console.warn('Failed to persist prefs', err);
  }
}

function toggleTheme(): void {
  const cur = document.documentElement.dataset.theme;
  const next: 'light' | 'dark' = cur === 'dark' ? 'light' : 'dark';
  state.prefs.theme = next;
  applyTheme();
  void savePrefs({ theme: next });
}

function toggleWidth(): void {
  const next: 'comfortable' | 'full' = state.prefs.width === 'comfortable' ? 'full' : 'comfortable';
  state.prefs.width = next;
  applyWidth();
  void savePrefs({ width: next });
}

function renderDoc(html: string): void {
  const clean = DOMPurify.sanitize(html, {
    ADD_TAGS: ['details', 'summary'] as string[],
    ADD_ATTR: ['data-block-id', 'data-thread-id'] as string[],
  });
  document.getElementById('doc')!.innerHTML = clean;
}

// Wrap the anchor quote for every thread/note in a <mark> inside the doc so
// the user can see which substring each thread originated from. Safe to call
// repeatedly — existing highlights are stripped first, so this function is
// idempotent across doc re-renders and thread state changes.
function applyQuoteHighlights(): void {
  // Unwrap previous highlights and re-merge the text nodes they split.
  const prev = document.querySelectorAll<HTMLElement>('.quote-highlight');
  const touchedParents = new Set<Node>();
  for (const mark of prev) {
    const parent = mark.parentNode;
    if (!parent) continue;
    while (mark.firstChild) parent.insertBefore(mark.firstChild, mark);
    parent.removeChild(mark);
    touchedParents.add(parent);
  }
  for (const p of touchedParents) (p as Element).normalize?.();

  const all = [...state.threads.values(), ...state.archived.values()];
  for (const t of all) {
    if (!t.anchor.quote) continue;
    const block = document.querySelector<HTMLElement>(
      `[data-block-id="${t.anchor.blockId}"]`,
    );
    if (!block) continue;
    highlightNthOccurrence(block, t.anchor.quote, t.anchor.occurrence ?? 1, t);
  }
}

// Walk text nodes inside `block` until we land inside the Nth occurrence of
// `quote` and wrap that range in a <mark>. Only quotes that fall inside a
// single text node are wrapped — cross-element quotes (e.g. selection that
// crosses a `<code>` span) are skipped silently because `surroundContents`
// rejects them; the thread card still renders its “quote” label, the doc
// just won't carry an inline highlight for it.
//
// Threads that share the same blockId + quote piggyback onto the mark created
// by the first thread — nesting <mark>s would either fail surroundContents or
// produce visually stacked highlights, and attaching multiple ids lets the
// click handler scroll to any of the owning threads.
function highlightNthOccurrence(
  block: HTMLElement,
  quote: string,
  occurrence: number,
  thread: Thread,
): void {
  const walker = document.createTreeWalker(block, NodeFilter.SHOW_TEXT, {
    acceptNode(node) {
      // Skip text inside the block-plus button SVG (none today, but defensive).
      // We DO walk into existing .quote-highlight marks so threads sharing a
      // quote with an already-wrapped thread can still find their target.
      const parent = (node as Text).parentElement;
      if (parent?.closest('.block-plus')) return NodeFilter.FILTER_REJECT;
      return NodeFilter.FILTER_ACCEPT;
    },
  });
  let remaining = occurrence;
  let node: Text | null;
  while ((node = walker.nextNode() as Text | null)) {
    let idx = 0;
    while ((idx = node.data.indexOf(quote, idx)) !== -1) {
      remaining -= 1;
      if (remaining === 0) {
        const existing = node.parentElement?.closest<HTMLElement>('.quote-highlight');
        if (existing) {
          attachThreadToQuoteMark(existing, thread);
          return;
        }
        const range = document.createRange();
        range.setStart(node, idx);
        range.setEnd(node, idx + quote.length);
        const mark = document.createElement('mark');
        mark.className = `quote-highlight quote-highlight-${thread.kind}`;
        mark.title = thread.kind === 'note' ? 'Note anchor' : 'Thread anchor';
        try {
          range.surroundContents(mark);
        } catch {
          // Range crossed element boundaries — give up on this quote.
          return;
        }
        attachThreadToQuoteMark(mark, thread);
        return;
      }
      idx += quote.length;
    }
  }
}

// Record `thread` as one of the owners of `mark` and install a click handler
// once. When several threads share the same quote, clicking the mark scrolls
// to the first owner whose card is still in the DOM.
function attachThreadToQuoteMark(mark: HTMLElement, thread: Thread): void {
  const current = (mark.dataset.threadIds ?? '').split(',').filter(Boolean);
  if (!current.includes(thread.id)) current.push(thread.id);
  mark.dataset.threadIds = current.join(',');
  // Keep data-thread-id in sync with the first owner so the CSS selector
  // `#doc [data-thread-id]` and any external readers still match.
  if (!mark.dataset.threadId) mark.dataset.threadId = thread.id;
  if (mark.dataset.clickBound) return;
  mark.dataset.clickBound = '1';
  mark.addEventListener('click', (e) => {
    e.stopPropagation();
    const ids = (mark.dataset.threadIds ?? '').split(',').filter(Boolean);
    for (const id of ids) {
      const card = document.getElementById(`thread-${id}`);
      if (!card) continue;
      card.scrollIntoView({ behavior: 'smooth', block: 'center' });
      card.classList.add('thread-flash');
      setTimeout(() => card.classList.remove('thread-flash'), 900);
      return;
    }
  });
}

function installBlockPluses(): void {
  for (const el of document.querySelectorAll<HTMLElement>('[data-block-id]')) {
    if (el.querySelector('.block-plus')) continue;
    const btn = document.createElement('button');
    btn.className = 'block-plus';
    btn.setAttribute('aria-label', 'Start thread on this block');
    btn.innerHTML = '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="M10 4v12M4 10h12" stroke="currentColor" stroke-width="2" stroke-linecap="round"/></svg>';
    btn.addEventListener('click', (e) => {
      e.stopPropagation();
      openComposer(el.dataset.blockId!, undefined);
    });
    el.appendChild(btn);
  }
  installRangeSelection();
}

let rangeSelectionInstalled = false;
function installRangeSelection(): void {
  if (rangeSelectionInstalled) return;
  rangeSelectionInstalled = true;
  document.getElementById('doc')!.addEventListener('mouseup', () => {
    const sel = window.getSelection();
    if (!sel || sel.isCollapsed) return;
    const range = sel.getRangeAt(0);
    const anchorBlock = range.startContainer.parentElement?.closest('[data-block-id]') as HTMLElement | null;
    if (!anchorBlock) return;
    const blockId = anchorBlock.dataset.blockId!;
    const quote = sel.toString();
    if (!quote.trim()) return;
    showFloatingComment(range, () => openComposer(blockId, quote));
  });
}

let floating: HTMLElement | null = null;
function showFloatingComment(range: Range, onClick: () => void): void {
  floating?.remove();
  const rect = range.getBoundingClientRect();
  const btn = document.createElement('button');
  btn.className = 'floating-comment';
  btn.innerHTML = '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="M4 6a2 2 0 0 1 2-2h8a2 2 0 0 1 2 2v6a2 2 0 0 1-2 2H9l-3 3v-3H6a2 2 0 0 1-2-2V6z" fill="currentColor"/></svg><span>Comment</span>';
  btn.style.top = `${rect.top - 44}px`;
  btn.style.left = `${rect.left}px`;
  btn.addEventListener('click', () => { onClick(); floating?.remove(); floating = null; });
  document.body.appendChild(btn);
  floating = btn;
  setTimeout(() => { floating?.remove(); floating = null; }, 5000);
}

// When a user highlights text inside an assistant message of an open thread,
// offer a one-click "Quote" button that drops the selection (each line prefixed
// with "| ") into that thread's reply textarea.
let threadQuoteSelectionInstalled = false;
function installThreadQuoteSelection(): void {
  if (threadQuoteSelectionInstalled) return;
  threadQuoteSelectionInstalled = true;
  document.addEventListener('mouseup', (e) => {
    // Clicking the floating Quote button also fires a mouseup that bubbles to
    // document. Without this guard we'd tear the button down (via the floating
    // singleton reset in showFloatingQuote) before its own click handler could
    // run — so the button appeared clickable but never inserted the quote.
    const target = e.target as Element | null;
    if (target?.closest('.floating-comment')) return;
    const sel = window.getSelection();
    if (!sel || sel.isCollapsed) return;
    const range = sel.getRangeAt(0);
    const start = range.startContainer.nodeType === 1
      ? (range.startContainer as Element)
      : range.startContainer.parentElement;
    const msg = start?.closest('.msg.assistant') as HTMLElement | null;
    if (!msg) return;
    const card = msg.closest('.thread-card') as HTMLElement | null;
    if (!card || card.classList.contains('resolved')) return;
    const text = sel.toString();
    if (!text.trim()) return;
    showFloatingQuote(range, card, text);
  });
}

function showFloatingQuote(range: Range, card: HTMLElement, text: string): void {
  floating?.remove();
  const rect = range.getBoundingClientRect();
  const btn = document.createElement('button');
  btn.className = 'floating-comment floating-quote';
  btn.innerHTML = '<svg viewBox="0 0 20 20" aria-hidden="true"><path d="M5 5h4v4H7c0 2 1 3 2 3v2c-3 0-4-2-4-5V5zm7 0h4v4h-2c0 2 1 3 2 3v2c-3 0-4-2-4-5V5z" fill="currentColor"/></svg><span>Quote</span>';
  btn.style.top = `${rect.top - 44}px`;
  btn.style.left = `${rect.left}px`;
  btn.addEventListener('click', () => {
    insertQuoteIntoReply(card, text);
    floating?.remove();
    floating = null;
    window.getSelection()?.removeAllRanges();
  });
  document.body.appendChild(btn);
  floating = btn;
  setTimeout(() => { floating?.remove(); floating = null; }, 5000);
}

function insertQuoteIntoReply(card: HTMLElement, text: string): void {
  const ta = card.querySelector<HTMLTextAreaElement>('.reply');
  if (!ta) return;
  const quoted = text.split('\n').map((l) => `| ${l}`).join('\n');
  const existing = ta.value;
  const separator = existing
    ? (existing.endsWith('\n\n') ? '' : existing.endsWith('\n') ? '\n' : '\n\n')
    : '';
  ta.value = existing + separator + quoted + '\n\n';
  ta.focus();
  ta.selectionStart = ta.selectionEnd = ta.value.length;
  autogrowTextarea(ta);
}

function openComposer(blockId: string, quote: string | undefined): void {
  const anchor = document.querySelector<HTMLElement>(`[data-block-id="${blockId}"]`);
  if (!anchor) return;
  const existing = anchor.nextElementSibling;
  if (existing?.classList.contains('composer')) return;
  const box = document.createElement('div');
  box.className = 'composer thread-card';
  box.innerHTML = `
    ${quote ? `<blockquote class="quote">${escapeHtml(quote)}</blockquote>` : ''}
    <textarea rows="3" placeholder="Message Claude…  Enter to send, Shift+Enter for newline. Or add a standalone note."></textarea>
    <div class="composer-actions">
      <button class="btn btn-primary send">Send to Claude</button>
      <button class="btn note">Add note</button>
      <button class="btn cancel">Cancel</button>
    </div>`;
  anchor.after(box);
  const ta = box.querySelector('textarea')!;
  // Defer focus to the next frame. Focusing synchronously inside the `+`-button
  // click handler sometimes left the textarea without a visible caret because
  // the click cycle re-evaluated focus after our call returned.
  requestAnimationFrame(() => {
    ta.focus({ preventScroll: true });
    ta.scrollIntoView({ block: 'center', behavior: 'smooth' });
  });
  ta.addEventListener('keydown', (e) => {
    // Enter submits; Shift+Enter inserts a newline. Cmd/Ctrl+Enter kept for back-compat.
    if (e.key === 'Enter' && !e.shiftKey && !e.isComposing) {
      e.preventDefault();
      (box.querySelector('.send') as HTMLButtonElement).click();
    }
  });
  const sendBtn = box.querySelector('.send') as HTMLButtonElement;
  const noteBtn = box.querySelector('.note') as HTMLButtonElement;
  const cancelBtn = box.querySelector('.cancel') as HTMLButtonElement;
  const submit = async (kind: 'thread' | 'note'): Promise<void> => {
    const message = ta.value.trim(); if (!message) return;
    sendBtn.disabled = true;
    noteBtn.disabled = true;
    cancelBtn.disabled = true;
    ta.disabled = true;
    const prevError = box.querySelector('.composer-error');
    if (prevError) prevError.remove();
    try {
      await createThread(blockId, quote, message, kind);
      box.remove();
    } catch (err) {
      const errDiv = document.createElement('div');
      errDiv.className = 'composer-error';
      errDiv.textContent = `⚠ Failed to send: ${err instanceof Error ? err.message : String(err)}`;
      box.appendChild(errDiv);
      sendBtn.disabled = false;
      noteBtn.disabled = false;
      cancelBtn.disabled = false;
      ta.disabled = false;
      ta.focus();
    }
  };
  sendBtn.addEventListener('click', () => void submit('thread'));
  noteBtn.addEventListener('click', () => void submit('note'));
  cancelBtn.addEventListener('click', () => box.remove());
}

async function createThread(
  blockId: string,
  quote: string | undefined,
  message: string,
  kind: 'thread' | 'note',
): Promise<void> {
  const r = await fetch('/api/threads', {
    method: 'POST', headers: { 'content-type': 'application/json' },
    body: JSON.stringify({ anchor: { blockId, quote, occurrence: 1 }, message, kind }),
  });
  if (!r.ok) throw new Error(`server responded ${r.status}`);
  const { threadId } = (await r.json()) as { threadId: string };
  const thread: Thread = {
    id: threadId, kind, status: 'open',
    anchor: { blockId, quote, occurrence: 1 },
    messages: [{ role: 'user', text: message, ts: new Date().toISOString() }],
    createdAt: new Date().toISOString(),
  };
  state.threads.set(threadId, thread);
  state.activeThreadId = threadId;
  renderThread(thread);
  applyQuoteHighlights();
}

// Sizes a textarea to its content up to the CSS max-height. Called on `input`
// and after clearing so the reply field grows with Shift+Enter and shrinks
// back to one line after a message is sent.
function autogrowTextarea(ta: HTMLTextAreaElement): void {
  ta.style.height = 'auto';
  ta.style.height = `${ta.scrollHeight}px`;
}

function renderThread(thread: Thread): void {
  const anchor = document.querySelector<HTMLElement>(`[data-block-id="${thread.anchor.blockId}"]`);
  if (!anchor) return;
  let card = document.getElementById(`thread-${thread.id}`) as HTMLElement | null;
  if (!card) {
    card = document.createElement('div');
    card.id = `thread-${thread.id}`;
    card.className = 'thread-card';
    if (thread.kind === 'note') card.classList.add('note-card');
    let cursor: Element = anchor;
    while (cursor.nextElementSibling?.classList.contains('thread-card')) {
      cursor = cursor.nextElementSibling;
    }
    cursor.after(card);
  }
  if (thread.status === 'closed' || thread.status === 'archived') {
    card.classList.add('resolved');
    const label = thread.kind === 'note' ? 'Note' : 'Resolved';
    // Edit is only offered for in-session closed *threads*. Pre-archived blocks
    // loaded from disk (status==='archived') and notes stay read-only.
    const editable = thread.status === 'closed' && thread.kind !== 'note';
    // Archived (loaded-from-disk) blocks can be deleted — this rips the
    // <details> out of the doc. In-session closed threads can be un-done by
    // editing their conclusion, so we don't expose a destructive delete here.
    const deletableArchive = thread.status === 'archived';
    card.innerHTML = `
      <div class="thread-resolved">
        <span class="check">✓</span>
        <span class="resolved-text">${label} — <span class="resolved-conclusion">${renderMarkdown(thread.conclusion ?? '')}</span></span>
        ${editable ? '<button class="btn btn-ghost edit-conclusion-btn">Edit</button>' : ''}
        ${deletableArchive ? '<button class="btn btn-ghost delete-archived-btn">Delete</button>' : ''}
      </div>`;
    if (editable) {
      (card.querySelector('.edit-conclusion-btn') as HTMLButtonElement).addEventListener('click', () => {
        openConclusionEditor(thread, card);
      });
    }
    if (deletableArchive) {
      const delBtn = card.querySelector('.delete-archived-btn') as HTMLButtonElement;
      delBtn.addEventListener('click', async () => {
        if (!confirm('Delete this archived thread?\n\nThe entire <details> block is removed from the doc. This cannot be undone.')) return;
        delBtn.disabled = true;
        try {
          const r = await fetch(`/api/threads/${thread.id}`, { method: 'DELETE' });
          if (!r.ok) throw new Error(`server responded ${r.status}`);
        } catch (err) {
          showCardError(card, `Failed to delete archived thread: ${err instanceof Error ? err.message : String(err)}`);
          delBtn.disabled = false;
        }
      });
    }
    return;
  }
  card.classList.remove('resolved');
  if (thread.kind === 'note') {
    renderNoteCard(card, thread);
    return;
  }
  card.innerHTML = `
    <div class="thread-header">
      <div class="thread-label"><span class="thread-icon">💬</span> <strong>Thread</strong> <span class="anchor-quote">${thread.anchor.quote ? `“${escapeHtml(thread.anchor.quote)}”` : 'entire block'}</span></div>
      <div class="thread-actions">
        <button class="btn btn-ghost to-note-btn" title="Collapse this thread into a single note">↩ To note</button>
      </div>
    </div>
    <div class="messages"></div>
    <div class="reply-row">
      <textarea class="reply" rows="1" placeholder="Reply…  Enter to send, Shift+Enter for newline"></textarea>
      <button class="btn btn-primary send">Send</button>
      <button class="btn btn-ghost close-btn">Close thread</button>
      <button class="btn btn-ghost delete-btn">Delete</button>
    </div>`;
  const messagesEl = card.querySelector<HTMLElement>('.messages')!;
  for (const m of thread.messages) {
    const el = document.createElement('div');
    el.className = `msg ${m.role}`;
    if (m.role === 'assistant') el.innerHTML = renderMarkdown(m.text);
    else el.textContent = m.text;
    messagesEl.appendChild(el);
  }
  const streamEl = document.createElement('div');
  streamEl.className = 'streaming';
  messagesEl.appendChild(streamEl);
  (card.querySelector('.send') as HTMLButtonElement).addEventListener('click', async () => {
    const input = card!.querySelector<HTMLTextAreaElement>('.reply')!;
    const text = input.value.trim(); if (!text) return;
    input.value = '';
    autogrowTextarea(input);
    appendMsg(card!, 'user', text);
    const current = state.threads.get(thread.id);
    if (current) current.messages.push({ role: 'user', text, ts: new Date().toISOString() });
    try {
      const r = await fetch(`/api/threads/${thread.id}/messages`, {
        method: 'POST', headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ message: text }),
      });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      onMessageError({ threadId: thread.id, error: err instanceof Error ? err.message : String(err) });
    }
  });
  const replyTa = card.querySelector('.reply') as HTMLTextAreaElement;
  replyTa.addEventListener('keydown', (e) => {
    // Enter submits; Shift+Enter inserts a newline. Cmd/Ctrl+Enter kept for back-compat.
    if (e.key === 'Enter' && !e.shiftKey && !e.isComposing) {
      e.preventDefault();
      (card!.querySelector('.send') as HTMLButtonElement).click();
    }
  });
  replyTa.addEventListener('input', () => autogrowTextarea(replyTa));
  const toNoteBtn = card.querySelector('.to-note-btn') as HTMLButtonElement;
  toNoteBtn.addEventListener('click', async () => {
    if (!confirm('Convert this thread into a note?\n\nThe full transcript is discarded. The note keeps the last Claude reply (or the last user message if there is none yet).')) return;
    toNoteBtn.disabled = true;
    try {
      const r = await fetch(`/api/threads/${thread.id}/convert`, {
        method: 'POST', headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ to: 'note' }),
      });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      showCardError(card, `Failed to convert to note: ${err instanceof Error ? err.message : String(err)}`);
      toNoteBtn.disabled = false;
    }
  });
  const deleteBtn = card.querySelector('.delete-btn') as HTMLButtonElement;
  deleteBtn.addEventListener('click', async () => {
    if (!confirm('Delete this thread?\n\nThe transcript will be discarded and NOT archived into the doc.')) return;
    deleteBtn.disabled = true;
    try {
      const r = await fetch(`/api/threads/${thread.id}`, { method: 'DELETE' });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      showCardError(card, `Failed to delete thread: ${err instanceof Error ? err.message : String(err)}`);
      deleteBtn.disabled = false;
    }
  });
  const closeBtns = card.querySelectorAll<HTMLButtonElement>('.close-btn');
  for (const btn of closeBtns) {
    btn.addEventListener('click', async () => {
      for (const b of closeBtns) {
        b.disabled = true;
        if (b.dataset.origLabel === undefined) b.dataset.origLabel = b.textContent ?? 'Close thread';
        b.innerHTML = '<span class="spinner" aria-hidden="true"></span> Summarising…';
      }
      try {
        await proposeAndClose(thread);
      } catch (err) {
        console.error('proposeAndClose failed', err);
        const msg = err instanceof Error ? err.message : String(err);
        const errDiv = document.createElement('div');
        errDiv.className = 'thread-error';
        errDiv.textContent = `⚠ Failed to close thread: ${msg}`;
        card!.appendChild(errDiv);
        for (const b of closeBtns) {
          b.disabled = false;
          b.textContent = b.dataset.origLabel ?? 'Close thread';
        }
      }
    });
  }
}

function renderNoteCard(card: HTMLElement, thread: Thread): void {
  const noteText = thread.messages[0]?.text ?? '';
  card.innerHTML = `
    <div class="thread-header">
      <div class="thread-label"><span class="thread-icon">📝</span> <strong>Note</strong> <span class="anchor-quote">${thread.anchor.quote ? `“${escapeHtml(thread.anchor.quote)}”` : 'entire block'}</span></div>
      <div class="thread-actions">
        <button class="btn btn-ghost edit-note-btn">Edit</button>
        <button class="btn btn-ghost to-thread-btn" title="Open a Claude conversation with this note as the first message">↪ Ask Claude</button>
        <button class="btn btn-ghost delete-btn">Delete note</button>
      </div>
    </div>
    <div class="messages">
      <div class="msg note">${renderMarkdown(noteText)}</div>
    </div>`;

  const deleteBtn = card.querySelector('.delete-btn') as HTMLButtonElement;
  deleteBtn.addEventListener('click', async () => {
    if (!confirm('Delete this note?\n\nThe text will be discarded and NOT archived into the doc.')) return;
    deleteBtn.disabled = true;
    try {
      const r = await fetch(`/api/threads/${thread.id}`, { method: 'DELETE' });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      showCardError(card, `Failed to delete note: ${err instanceof Error ? err.message : String(err)}`);
      deleteBtn.disabled = false;
    }
  });

  const toThreadBtn = card.querySelector('.to-thread-btn') as HTMLButtonElement;
  toThreadBtn.addEventListener('click', async () => {
    toThreadBtn.disabled = true;
    try {
      const r = await fetch(`/api/threads/${thread.id}/convert`, {
        method: 'POST', headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ to: 'thread' }),
      });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      showCardError(card, `Failed to ask Claude: ${err instanceof Error ? err.message : String(err)}`);
      toThreadBtn.disabled = false;
    }
  });

  const editBtn = card.querySelector('.edit-note-btn') as HTMLButtonElement;
  editBtn.addEventListener('click', () => {
    const msgEl = card.querySelector<HTMLElement>('.msg.note')!;
    msgEl.innerHTML = `
      <textarea class="note-edit-textarea" rows="3">${escapeHtml(noteText)}</textarea>
      <div class="note-edit-actions">
        <button class="btn btn-primary save-note-btn">Save</button>
        <button class="btn cancel-note-btn">Cancel</button>
      </div>`;
    const ta = msgEl.querySelector('textarea') as HTMLTextAreaElement;
    requestAnimationFrame(() => { ta.focus(); ta.selectionStart = ta.selectionEnd = ta.value.length; });
    autogrowTextarea(ta);
    ta.addEventListener('input', () => autogrowTextarea(ta));
    ta.addEventListener('keydown', (e) => {
      if (e.key === 'Enter' && (e.metaKey || e.ctrlKey)) {
        e.preventDefault();
        (msgEl.querySelector('.save-note-btn') as HTMLButtonElement).click();
      }
      if (e.key === 'Escape') (msgEl.querySelector('.cancel-note-btn') as HTMLButtonElement).click();
    });
    const saveBtn = msgEl.querySelector('.save-note-btn') as HTMLButtonElement;
    saveBtn.addEventListener('click', async () => {
      saveBtn.disabled = true;
      const next = ta.value;
      try {
        const r = await fetch(`/api/threads/${thread.id}/note`, {
          method: 'PATCH', headers: { 'content-type': 'application/json' },
          body: JSON.stringify({ message: next }),
        });
        if (!r.ok) throw new Error(`server responded ${r.status}`);
      } catch (err) {
        showCardError(card, `Failed to save note: ${err instanceof Error ? err.message : String(err)}`);
        saveBtn.disabled = false;
      }
    });
    (msgEl.querySelector('.cancel-note-btn') as HTMLButtonElement).addEventListener('click', () => {
      msgEl.innerHTML = renderMarkdown(noteText);
    });
  });
}

function showCardError(card: HTMLElement, message: string): void {
  const prev = card.querySelector('.thread-error');
  if (prev) prev.remove();
  const errDiv = document.createElement('div');
  errDiv.className = 'thread-error';
  errDiv.textContent = `⚠ ${message}`;
  card.appendChild(errDiv);
}

// Swap the resolved chip for an inline conclusion editor (textarea + live
// markdown preview + "Use last Claude reply"). Save PUTs the new conclusion,
// which rewrites the archived <details> block server-side — the subsequent
// `thread.updated` event re-renders the chip in place.
function openConclusionEditor(thread: Thread, card: HTMLElement): void {
  const current = thread.conclusion ?? '';
  card.classList.remove('resolved');
  card.innerHTML = `
    <div class="conclusion-edit">
      <div class="conclusion-label">Edit conclusion</div>
      <textarea rows="5">${escapeHtml(current)}</textarea>
      <div class="conclusion-preview-label">Preview</div>
      <div class="conclusion-preview"></div>
      <div class="conclusion-actions">
        <button class="btn btn-primary save-conclusion-btn">Save</button>
        <button class="btn use-last-reply">Use last Claude reply</button>
        <button class="btn cancel-conclusion-btn">Cancel</button>
      </div>
    </div>`;
  const section = card.querySelector('.conclusion-edit') as HTMLElement;
  const textarea = section.querySelector('textarea') as HTMLTextAreaElement;
  const preview = section.querySelector('.conclusion-preview') as HTMLElement;
  const refreshPreview = (): void => { preview.innerHTML = renderMarkdown(textarea.value); };
  textarea.addEventListener('input', refreshPreview);
  refreshPreview();
  requestAnimationFrame(() => textarea.focus());

  const useLastBtn = section.querySelector('.use-last-reply') as HTMLButtonElement;
  const hasLast = [...thread.messages].reverse().some((m) => m.role === 'assistant');
  if (!hasLast) useLastBtn.disabled = true;
  useLastBtn.addEventListener('click', () => {
    const lastAssistant = [...thread.messages].reverse().find((m) => m.role === 'assistant');
    if (!lastAssistant) return;
    textarea.value = lastAssistant.text;
    refreshPreview();
    textarea.focus();
  });

  (section.querySelector('.save-conclusion-btn') as HTMLButtonElement).addEventListener('click', async () => {
    (section.querySelector('.save-conclusion-btn') as HTMLButtonElement).disabled = true;
    try {
      const r = await fetch(`/api/threads/${thread.id}/conclusion`, {
        method: 'PUT', headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ conclusion: textarea.value }),
      });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      showCardError(card, `Failed to save conclusion: ${err instanceof Error ? err.message : String(err)}`);
      (section.querySelector('.save-conclusion-btn') as HTMLButtonElement).disabled = false;
    }
  });

  (section.querySelector('.cancel-conclusion-btn') as HTMLButtonElement).addEventListener('click', () => {
    renderThread(thread);
  });
}

function appendMsg(card: HTMLElement, role: 'user' | 'assistant', text: string): void {
  const container = card.querySelector('.messages')!;
  const div = document.createElement('div');
  div.className = `msg ${role}`;
  if (role === 'assistant') div.innerHTML = renderMarkdown(text);
  else div.textContent = text;
  container.insertBefore(div, container.querySelector('.streaming'));
}

function onDelta(evt: { threadId: string; delta: string }): void {
  const card = document.getElementById(`thread-${evt.threadId}`);
  if (!card) return;
  const stream = card.querySelector<HTMLElement>('.streaming')!;
  if (!stream.classList.contains('msg')) {
    stream.classList.add('msg', 'assistant');
  }
  // Accumulate the raw markdown in a dataset attr and re-render on every
  // delta. marked tolerates half-finished markdown (unclosed fences, dangling
  // list markers, etc.), so partial renders degrade gracefully.
  const next = (stream.dataset.raw ?? '') + evt.delta;
  stream.dataset.raw = next;
  stream.innerHTML = renderMarkdown(next);
}

function onDone(evt: { threadId: string; message: { role: 'assistant'; text: string } }): void {
  const card = document.getElementById(`thread-${evt.threadId}`);
  if (!card) return;
  const stream = card.querySelector<HTMLElement>('.streaming')!;
  if (!stream.classList.contains('msg')) {
    stream.classList.add('msg', 'assistant');
  }
  // Replace the streaming text with the markdown-rendered final message.
  stream.innerHTML = renderMarkdown(evt.message.text);
  stream.classList.remove('streaming');
  const next = document.createElement('div'); next.className = 'streaming';
  card.querySelector('.messages')!.appendChild(next);
  const thread = state.threads.get(evt.threadId);
  if (thread) thread.messages.push({ role: 'assistant', text: evt.message.text, ts: new Date().toISOString() });
}

function onMessageError(evt: { threadId: string; error: string }): void {
  const card = document.getElementById(`thread-${evt.threadId}`);
  if (!card) return;
  const stream = card.querySelector<HTMLElement>('.streaming');
  if (!stream) return;
  if (!stream.classList.contains('msg')) {
    stream.classList.add('msg', 'assistant');
  }
  stream.classList.remove('streaming');
  stream.classList.add('error');
  stream.textContent = `⚠ Agent reply failed: ${evt.error}`;
  const next = document.createElement('div'); next.className = 'streaming';
  card.querySelector('.messages')!.appendChild(next);
}

async function proposeAndClose(thread: Thread): Promise<void> {
  const r = await fetch(`/api/threads/${thread.id}/propose-conclusion`, { method: 'POST' });
  if (!r.ok) throw new Error(`server responded ${r.status}`);
}

function onConclusion(evt: { threadId: string; conclusion: string }): void {
  const card = document.getElementById(`thread-${evt.threadId}`);
  if (!card) return;
  const t = state.threads.get(evt.threadId);
  if (t && t.status === 'closed') return;
  // Spinner / "Summarising…" label did its job — swap close-btns back to
  // their original text (they stay disabled while the editor is open).
  for (const b of card.querySelectorAll<HTMLButtonElement>('.close-btn')) {
    b.textContent = b.dataset.origLabel ?? 'Close thread';
  }
  const existing = card.querySelector<HTMLElement>('.conclusion-edit');
  if (existing) {
    const textarea = existing.querySelector<HTMLTextAreaElement>('textarea');
    if (textarea) {
      textarea.value = evt.conclusion;
      textarea.dispatchEvent(new Event('input'));
    }
    return;
  }
  const section = document.createElement('div');
  section.className = 'conclusion-edit';
  section.innerHTML = `
    <div class="conclusion-label">Claude's proposed conclusion (edit before saving):</div>
    <textarea rows="5">${escapeHtml(evt.conclusion)}</textarea>
    <div class="conclusion-preview-label">Preview</div>
    <div class="conclusion-preview"></div>
    <div class="conclusion-actions">
      <button class="btn btn-primary save">Save conclusion</button>
      <button class="btn use-last-reply">Use last Claude reply</button>
      <button class="btn cancel">Cancel</button>
    </div>`;
  card.appendChild(section);
  const textarea = section.querySelector('textarea') as HTMLTextAreaElement;
  const preview = section.querySelector('.conclusion-preview') as HTMLElement;
  const refreshPreview = (): void => { preview.innerHTML = renderMarkdown(textarea.value); };
  textarea.addEventListener('input', refreshPreview);
  refreshPreview();
  const useLastBtn = section.querySelector('.use-last-reply') as HTMLButtonElement;
  useLastBtn.addEventListener('click', () => {
    const lastAssistant = [...(state.threads.get(evt.threadId)?.messages ?? [])].reverse().find((m) => m.role === 'assistant');
    if (!lastAssistant) return;
    textarea.value = lastAssistant.text;
    refreshPreview();
    textarea.focus();
  });
  (section.querySelector('.save') as HTMLButtonElement).addEventListener('click', async () => {
    await fetch(`/api/threads/${evt.threadId}/close`, {
      method: 'POST', headers: { 'content-type': 'application/json' },
      body: JSON.stringify({ conclusion: textarea.value }),
    });
  });
  (section.querySelector('.cancel') as HTMLButtonElement).addEventListener('click', async () => {
    // OK → close thread with empty conclusion; Cancel → keep editor open to revisit.
    const discard = confirm(
      'Discard this proposed conclusion and close the thread without one?\n\n' +
      'OK — close thread without a conclusion (proposal is lost)\n' +
      'Cancel — revisit the proposed conclusion',
    );
    if (!discard) return;
    section.remove();
    try {
      const r = await fetch(`/api/threads/${evt.threadId}/close`, {
        method: 'POST', headers: { 'content-type': 'application/json' },
        body: JSON.stringify({ conclusion: '' }),
      });
      if (!r.ok) throw new Error(`server responded ${r.status}`);
    } catch (err) {
      const msg = err instanceof Error ? err.message : String(err);
      const errDiv = document.createElement('div');
      errDiv.className = 'thread-error';
      errDiv.textContent = `⚠ Failed to close thread: ${msg}`;
      card.appendChild(errDiv);
      // Re-enable close-btns so the user can try again.
      for (const b of card.querySelectorAll<HTMLButtonElement>('.close-btn')) {
        b.disabled = false;
      }
    }
  });
}

function onClosed(evt: { threadId: string; conclusion?: string }): void {
  const t = state.threads.get(evt.threadId); if (!t) return;
  t.status = 'closed';
  if (evt.conclusion !== undefined) t.conclusion = evt.conclusion;
  renderThread(t);
}

function onDeleted(evt: { threadId: string }): void {
  // Live and archived threads live in separate maps — drop from either.
  state.threads.delete(evt.threadId);
  state.archived.delete(evt.threadId);
  const card = document.getElementById(`thread-${evt.threadId}`);
  if (card) card.remove();
  applyQuoteHighlights();
}

// Replaces our local Thread with the server copy and re-renders the card.
// Used after note edits, conclusion edits, and thread↔note conversions. The
// conversion case is why we re-apply quote highlights: the <mark> class
// encodes `kind` (quote-highlight-thread vs quote-highlight-note), so the
// highlight color must follow the new kind.
function onUpdated(evt: { threadId: string; thread: Thread }): void {
  state.threads.set(evt.threadId, evt.thread);
  renderThread(evt.thread);
  applyQuoteHighlights();
}

function onDocUpdated(evt: { html: string; blockIds: string[]; title?: string; archivedThreads?: Thread[] }): void {
  const scroll = window.scrollY;
  if (evt.title !== undefined) applyTitle(evt.title);
  renderDoc(evt.html);
  installBlockPluses();
  // When the server re-parsed archives (e.g. after deleting one), the remaining
  // threads get fresh `archived-N` ids — replace our mirror so future deletes
  // point at the right blocks.
  if (evt.archivedThreads) {
    state.archived.clear();
    for (const t of evt.archivedThreads) state.archived.set(t.id, t);
  }
  for (const t of state.threads.values()) renderThread(t);
  for (const t of state.archived.values()) renderThread(t);
  applyQuoteHighlights();
  window.scrollTo({ top: scroll });
}

function applyTitle(title: string): void {
  const clean = title.trim() || 'Markdown Preview';
  document.title = `${clean} — Markdown Preview`;
  const el = document.getElementById('title');
  if (el) el.textContent = clean;
}

function onFinished(_evt: { result: unknown }): void {
  document.body.innerHTML = '<div class="finished"><h2>Server stopped</h2><p>You can close this tab.</p></div>';
  try { window.close(); } catch { /* ignore */ }
}

function renderExistingThreads(): void {
  for (const t of state.threads.values()) renderThread(t);
  for (const t of state.archived.values()) renderThread(t);
  applyQuoteHighlights();
}

async function finish(): Promise<void> {
  const open = [...state.threads.values()].filter((t) => t.status === 'open');
  if (open.length > 0) {
    const openThreads = open.filter((t) => t.kind !== 'note').length;
    const openNotes = open.filter((t) => t.kind === 'note').length;
    const parts: string[] = [];
    if (openThreads > 0) parts.push(`${openThreads} open thread(s) will be auto-closed now and appended to the doc with a Claude-generated conclusion`);
    if (openNotes > 0) parts.push(`${openNotes} open note(s) will be appended to the doc as-is`);
    if (!confirm(`Finishing. ${parts.join('; ')}. Any threads you already closed this session are already in the doc. Continue?`)) return;
  }
  await fetch('/api/finish', { method: 'POST' });
}

function escapeHtml(s: string): string {
  return s.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;');
}
