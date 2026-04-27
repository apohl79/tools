// src/agent.ts
// SDK version: @anthropic-ai/claude-agent-sdk@0.2.0
// Verified types: query(), Options (systemPrompt, tools, canUseTool, includePartialMessages),
//   PermissionResult ({behavior:'allow',updatedInput}|{behavior:'deny',message}),
//   SDKMessage union (stream_event for deltas, assistant for final, result for terminal)
import type { ThreadMessage } from './types.ts';
import { query, type Options } from '@anthropic-ai/claude-agent-sdk';

export interface AgentFactoryOptions {
  systemPreamble: string;
  tools: string[]; // allow-list
}

export interface StreamChunk {
  type: 'delta' | 'done';
  text: string;
}

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

// Tools permitted in inline-discussion threads. WebFetch excluded (SSRF risk).
const ALLOWED_TOOLS = ['Read', 'Grep', 'Glob', 'WebSearch'] as const;

export interface DispatchState {
  accumulated: string;
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
    if (event?.['type'] === 'content_block_delta') {
      const delta = event['delta'] as Record<string, unknown> | undefined;
      if (delta?.['type'] === 'text_delta' && typeof delta['text'] === 'string') {
        state.accumulated += delta['text'];
        chunks.push({ type: 'delta', text: delta['text'] });
      }
    }
    return { chunks, endOfTurn: false };
  }

  if (e['type'] === 'result') {
    const subtype = e['subtype'];
    const finalText =
      subtype === 'success' && typeof e['result'] === 'string' && e['result']
        ? (e['result'] as string)
        : state.accumulated;
    chunks.push({ type: 'done', text: finalText });
    state.accumulated = '';
    return { chunks, endOfTurn: true };
  }

  // SDKAssistantMessage and other intermediate messages are ignored —
  // text content already arrives via stream_event deltas.
  return { chunks, endOfTurn: false };
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
