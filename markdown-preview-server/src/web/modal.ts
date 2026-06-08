// src/web/modal.ts
//
// In-page modal component. Two surfaces:
//   modalConfirm({…}) → Promise<boolean>     user-driven OK/Cancel
//   modalStatus({…})  → ModalStatusHandle    server-driven blocking overlay
//
// Both share one #modal-root mount point and one .modal-backdrop element.

export interface ModalConfirmOptions {
  title: string;
  body: string;             // \n\n splits paragraphs
  primaryLabel: string;
  primaryVariant?: 'danger' | 'default';
  secondaryLabel?: string;  // default 'Cancel'
}

export interface ModalChoiceOption {
  label: string;
  value: string;
  variant?: 'danger' | 'default';
}

export interface ModalChoiceOptions {
  title: string;
  body: string;                 // \n\n splits paragraphs
  options: ModalChoiceOption[]; // rendered left→right; the LAST is the focused primary
  cancelValue?: string | null;  // returned on Escape / backdrop click (default null)
}

export interface ModalStatusHandle {
  setStatus(text: string): void;
  setProgress(progress: ModalStatusProgress): void;
  setTasks(tasks: ModalStatusTask[]): void;
  setError(message: string): void;
  dismiss(): void;
}

export interface ModalStatusProgress {
  status?: string;
  percent?: number | null;
}

export interface ModalStatusTask {
  id?: string;
  label: string;
  state: 'active' | 'done' | 'error';
}

interface OpenModal {
  cleanup(): void;
}

let openModal: OpenModal | null = null;
let lastFocus: HTMLElement | null = null;

function ensureRoot(): HTMLElement {
  let root = document.getElementById('modal-root');
  if (!root) {
    root = document.createElement('div');
    root.id = 'modal-root';
    document.body.appendChild(root);
  }
  return root;
}

function bodyToParagraphs(body: string): HTMLElement {
  const wrap = document.createElement('div');
  wrap.className = 'modal-body';
  for (const para of body.split(/\n\n+/)) {
    const p = document.createElement('p');
    p.textContent = para;
    wrap.appendChild(p);
  }
  return wrap;
}

function setDocInert(on: boolean): void {
  const doc = document.getElementById('doc');
  const top = document.getElementById('topbar');
  const bot = document.getElementById('bottombar');
  for (const el of [doc, top, bot]) {
    if (!el) continue;
    if (on) el.setAttribute('inert', '');
    else el.removeAttribute('inert');
  }
}

function trapFocus(card: HTMLElement, e: KeyboardEvent): void {
  if (e.key !== 'Tab') return;
  const focusables = card.querySelectorAll<HTMLElement>('button, [tabindex]:not([tabindex="-1"])');
  if (focusables.length === 0) return;
  const first = focusables[0];
  const last = focusables[focusables.length - 1];
  if (!first || !last) return;
  if (e.shiftKey && document.activeElement === first) {
    e.preventDefault(); last.focus();
  } else if (!e.shiftKey && document.activeElement === last) {
    e.preventDefault(); first.focus();
  }
}

export function modalConfirm(opts: ModalConfirmOptions): Promise<boolean> {
  if (openModal) openModal.cleanup();
  return new Promise<boolean>((resolve) => {
    lastFocus = (document.activeElement as HTMLElement | null) ?? null;
    const root = ensureRoot();

    const backdrop = document.createElement('div');
    backdrop.className = 'modal-backdrop';

    const card = document.createElement('div');
    card.className = 'modal-card';
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-modal', 'true');

    const title = document.createElement('h2');
    title.className = 'modal-title';
    title.textContent = opts.title;
    card.appendChild(title);
    card.appendChild(bodyToParagraphs(opts.body));

    const actions = document.createElement('div');
    actions.className = 'modal-actions';
    const secondary = document.createElement('button');
    secondary.type = 'button';
    secondary.className = 'modal-btn modal-btn-secondary';
    secondary.textContent = opts.secondaryLabel ?? 'Cancel';
    const primary = document.createElement('button');
    primary.type = 'button';
    primary.className = `modal-btn modal-btn-primary${opts.primaryVariant === 'danger' ? ' modal-btn-danger' : ''}`;
    primary.textContent = opts.primaryLabel;
    actions.appendChild(secondary);
    actions.appendChild(primary);
    card.appendChild(actions);

    backdrop.appendChild(card);
    root.appendChild(backdrop);

    setDocInert(true);
    primary.focus();

    const onKey = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') { e.preventDefault(); finish(false); }
      else trapFocus(card, e);
    };
    const onBackdrop = (e: MouseEvent): void => {
      if (e.target === backdrop) finish(false);
    };

    function finish(result: boolean): void {
      cleanup();
      resolve(result);
    }
    function cleanup(): void {
      document.removeEventListener('keydown', onKey, true);
      backdrop.removeEventListener('mousedown', onBackdrop);
      backdrop.remove();
      setDocInert(false);
      if (lastFocus && lastFocus.isConnected) lastFocus.focus();
      openModal = null;
    }

    document.addEventListener('keydown', onKey, true);
    backdrop.addEventListener('mousedown', onBackdrop);
    primary.addEventListener('click', () => finish(true));
    secondary.addEventListener('click', () => finish(false));

    openModal = { cleanup: () => finish(false) };
  });
}

/**
 * Multi-button variant of modalConfirm. Renders one button per option
 * (left→right); the last option is the focused primary, the rest are
 * secondary unless an explicit `variant` is given. Escape / backdrop click
 * resolves with `cancelValue` (default null) so callers can distinguish an
 * explicit dismissal from any of the offered choices.
 */
export function modalChoice(opts: ModalChoiceOptions): Promise<string | null> {
  if (openModal) openModal.cleanup();
  return new Promise<string | null>((resolve) => {
    lastFocus = (document.activeElement as HTMLElement | null) ?? null;
    const root = ensureRoot();

    const backdrop = document.createElement('div');
    backdrop.className = 'modal-backdrop';

    const card = document.createElement('div');
    card.className = 'modal-card';
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-modal', 'true');

    const title = document.createElement('h2');
    title.className = 'modal-title';
    title.textContent = opts.title;
    card.appendChild(title);
    card.appendChild(bodyToParagraphs(opts.body));

    const actions = document.createElement('div');
    actions.className = 'modal-actions';
    const lastIdx = opts.options.length - 1;
    const buttons = opts.options.map((opt, i) => {
      const btn = document.createElement('button');
      btn.type = 'button';
      const isPrimary = i === lastIdx;
      const danger = opt.variant === 'danger';
      btn.className =
        `modal-btn ${isPrimary ? 'modal-btn-primary' : 'modal-btn-secondary'}` +
        (danger ? ' modal-btn-danger' : '');
      btn.textContent = opt.label;
      btn.addEventListener('click', () => finish(opt.value));
      actions.appendChild(btn);
      return btn;
    });
    card.appendChild(actions);

    backdrop.appendChild(card);
    root.appendChild(backdrop);

    setDocInert(true);
    buttons[lastIdx]?.focus();

    const cancelValue = opts.cancelValue ?? null;
    const onKey = (e: KeyboardEvent): void => {
      if (e.key === 'Escape') { e.preventDefault(); finish(cancelValue); }
      else trapFocus(card, e);
    };
    const onBackdrop = (e: MouseEvent): void => {
      if (e.target === backdrop) finish(cancelValue);
    };

    function finish(result: string | null): void {
      cleanup();
      resolve(result);
    }
    function cleanup(): void {
      document.removeEventListener('keydown', onKey, true);
      backdrop.removeEventListener('mousedown', onBackdrop);
      backdrop.remove();
      setDocInert(false);
      if (lastFocus && lastFocus.isConnected) lastFocus.focus();
      openModal = null;
    }

    document.addEventListener('keydown', onKey, true);
    backdrop.addEventListener('mousedown', onBackdrop);

    openModal = { cleanup: () => finish(cancelValue) };
  });
}

export function modalStatus(opts: {
  title: string;
  initialStatus: string;
  initialProgress?: ModalStatusProgress | null;
  initialTasks?: ModalStatusTask[];
}): ModalStatusHandle {
  if (openModal) openModal.cleanup();
  lastFocus = (document.activeElement as HTMLElement | null) ?? null;
  const root = ensureRoot();

  const backdrop = document.createElement('div');
  backdrop.className = 'modal-backdrop';
  const card = document.createElement('div');
  card.className = 'modal-card';
  card.setAttribute('role', 'dialog');
  card.setAttribute('aria-modal', 'true');

  const title = document.createElement('h2');
  title.className = 'modal-title';
  title.textContent = opts.title;
  card.appendChild(title);

  const statusWrap = document.createElement('div');
  statusWrap.className = 'modal-body modal-status-body';
  const taskList = document.createElement('ol');
  taskList.className = 'modal-task-list';
  taskList.setAttribute('role', 'list');
  statusWrap.appendChild(taskList);
  card.appendChild(statusWrap);

  const actions = document.createElement('div');
  actions.className = 'modal-actions';
  card.appendChild(actions);

  backdrop.appendChild(card);
  root.appendChild(backdrop);

  setDocInert(true);

  // Block ESC + backdrop until error.
  let inErrorState = false;
  const onKey = (e: KeyboardEvent): void => {
    if (e.key === 'Escape' && !inErrorState) e.preventDefault();
    if (inErrorState) trapFocus(card, e);
  };
  const onBackdrop = (e: MouseEvent): void => {
    if (e.target === backdrop && !inErrorState) e.preventDefault();
  };
  document.addEventListener('keydown', onKey, true);
  backdrop.addEventListener('mousedown', onBackdrop);

  function cleanup(): void {
    document.removeEventListener('keydown', onKey, true);
    backdrop.removeEventListener('mousedown', onBackdrop);
    backdrop.remove();
    setDocInert(false);
    if (lastFocus && lastFocus.isConnected) lastFocus.focus();
    openModal = null;
  }

  openModal = { cleanup };

  let localTaskSeq = 0;
  let currentTasks: ModalStatusTask[] = [];

  function cleanLabel(text: string): string {
    return text.replace(/\s+/g, ' ').trim() || 'Working...';
  }

  function renderTasks(tasks: ModalStatusTask[]): void {
    currentTasks = tasks.length > 0
      ? tasks.map((task, index) => ({
          id: task.id ?? `modal-task-${index + 1}`,
          label: cleanLabel(task.label),
          state: task.state,
        }))
      : [{ id: 'modal-task-initial', label: cleanLabel(opts.initialStatus), state: 'active' }];
    taskList.innerHTML = '';
    for (const task of currentTasks) {
      const item = document.createElement('li');
      item.className = `modal-task modal-task-${task.state}`;
      const marker = document.createElement('span');
      marker.className = 'modal-task-marker';
      marker.setAttribute('aria-hidden', 'true');
      if (task.state === 'active') {
        const spinner = document.createElement('span');
        spinner.className = 'modal-spinner modal-task-spinner';
        marker.appendChild(spinner);
      } else {
        marker.classList.add(task.state === 'done' ? 'modal-task-check' : 'modal-task-error');
        marker.textContent = task.state === 'done' ? '✓' : '!';
      }
      const label = document.createElement('span');
      label.className = 'modal-task-label';
      label.textContent = task.label;
      item.appendChild(marker);
      item.appendChild(label);
      taskList.appendChild(item);
    }
  }

  function appendStatus(text: string): void {
    const label = cleanLabel(text);
    const last = currentTasks[currentTasks.length - 1];
    if (last?.label === label) {
      renderTasks([...currentTasks.slice(0, -1), { ...last, state: 'active' }]);
      return;
    }
    const next = currentTasks.map((task) => task.state === 'active' ? { ...task, state: 'done' as const } : task);
    localTaskSeq += 1;
    next.push({ id: `modal-local-task-${localTaskSeq}`, label, state: 'active' });
    renderTasks(next);
  }

  function setProgress(progress: ModalStatusProgress): void {
    if (progress.status !== undefined) appendStatus(progress.status);
  }

  renderTasks(opts.initialTasks?.length
    ? opts.initialTasks
    : [{ id: 'modal-task-initial', label: opts.initialProgress?.status ?? opts.initialStatus, state: 'active' }]);
  if (opts.initialProgress) setProgress(opts.initialProgress);

  return {
    setStatus(text: string): void {
      appendStatus(text);
    },
    setProgress,
    setTasks(tasks: ModalStatusTask[]): void {
      renderTasks(tasks);
    },
    setError(message: string): void {
      inErrorState = true;
      const next = currentTasks.map((task) => task.state === 'active' ? { ...task, state: 'error' as const } : task);
      if (next.length === 0 || !next.some((task) => task.state === 'error')) {
        next.push({ id: 'modal-error-task', label: `Apply failed: ${message}`, state: 'error' });
      } else {
        const failed = next.find((task) => task.state === 'error');
        if (failed) failed.label = `Apply failed: ${message}`;
      }
      renderTasks(next);
      actions.innerHTML = '';
      const dismiss = document.createElement('button');
      dismiss.type = 'button';
      dismiss.className = 'modal-btn modal-btn-primary';
      dismiss.textContent = 'Dismiss';
      dismiss.addEventListener('click', cleanup);
      actions.appendChild(dismiss);
      dismiss.focus();
    },
    dismiss: cleanup,
  };
}
