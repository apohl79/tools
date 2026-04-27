export type BlockKind = 'heading' | 'paragraph' | 'code' | 'blockquote' | 'list' | 'table' | 'hr' | 'html';

export interface Block {
  id: string;
  kind: BlockKind;
  markdown: string;
  html: string;
}

export interface Anchor {
  blockId: string;
  quote?: string;
  occurrence?: number;
}

export type ThreadStatus = 'open' | 'closed' | 'archived';

export interface ThreadMessage {
  role: 'user' | 'assistant';
  text: string;
  ts: string;
}

export type ThreadKind = 'thread' | 'note';

export interface Thread {
  id: string;
  kind: ThreadKind;
  anchor: Anchor;
  status: ThreadStatus;
  messages: ThreadMessage[];
  conclusion?: string;
  createdAt: string;
  closedAt?: string;
  closedBy?: 'user' | 'auto';
  colorIndex?: number;
}

export interface FinishResult {
  docPath: string;
  conclusions: Array<{
    threadId: string;
    anchor: string;           // human label: quote or "entire block — <heading>"
    conclusion: string;
    closedBy: 'user' | 'auto';
  }>;
  threadCount: number;        // new this session
  archivedThreadCount: number;
  finishedAt: string;
}
