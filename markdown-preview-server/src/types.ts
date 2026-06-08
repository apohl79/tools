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
  mode: 'finish';
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

export interface ApplyResult {
  mode: 'apply';
  applyIndex: number;
  docPath: string;
  conclusions: FinishResult['conclusions'];
  threadCount: number;
  archivedThreadCount: number;
  finishedAt: string;
}

export interface ApplyProgress {
  status: string;
  percent: number | null;
  current?: number;
  total?: number;
  updatedAt: string;
}

export interface ApplyTask {
  id: string;
  label: string;
  state: 'active' | 'done' | 'error';
  updatedAt: string;
}
