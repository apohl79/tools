# Cmux integration Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add cmux support for the Doom Emacs `projects` workflow without changing the existing wezterm path. New `projects-cmux.el` backend, named-daemon-aware `tools/emacs` wrapper, and a small dispatcher in `config.el`. Selection is driven by `MY_PROJECTS_MODE` exported by the wrapper.

**Architecture:**
- `tools/emacs` wrapper detects cmux vs wezterm vs default, exports `MY_PROJECTS_MODE`, and uses a per-mode named Emacs daemon (`cmux`, `wezterm`, `default`).
- `.doom.d/config.el` reads `MY_PROJECTS_MODE` and loads exactly one of `projects.el` or `projects-cmux.el`.
- `.doom.d/projects-cmux.el` is fully self-contained: it copies the pure model from `projects.el` and replaces UI ops with cmux CLI calls. It does not `(require 'projects)`.
- `.doom.d/projects.el` stays untouched.

**Tech Stack:** POSIX shell (`sh`), Emacs Lisp (lexical-binding), Emacs ERT for unit tests, `cmux` CLI (`/opt/homebrew/bin/cmux`), `emacsclient -F` for frame-parameter-bound terminal frames.

**Source spec:** `docs/superpowers/specs/2026-05-06-cmux-integration-design.md` (commit `0760560`).

---

## File Structure

**Modify (existing files):**
- `tools/emacs` — add named-daemon detection and mode export. Keep current behavior when neither cmux nor wezterm is detected.
- `.doom.d/config.el` — add `my/projects-mode` defvar and conditional `load!`.

**Create (new files):**
- `.doom.d/projects-cmux.el` — self-contained cmux backend (~600 lines, including copied model). Public API mirrors `projects.el`: `projects-create`, `projects-delete`, `projects-rename`, `projects-switch`, `projects-set-layout`, plus cmux-specific commands `projects-cmux-select-workspace`, `projects-cmux-resync`.
- `.doom.d/test-projects-cmux.el` — ERT tests for `projects-cmux.el` (follows the existing `.doom.d/test-projects-*.el` convention).
- `tests/emacs-wrapper-test.sh` — POSIX shell tests for the wrapper.
- `tests/fixtures/capture-emacs.sh` — fake `emacs` binary for wrapper tests; records argv to `$CAPTURE_FILE`.
- `tests/fixtures/capture-emacsclient.sh` — fake `emacsclient`; records argv and exits 1 on the first call so the wrapper takes the daemon-start branch.
- `tests/fixtures/cmux-mock.sh` — fake `cmux` binary used by ERT tests; records argv to `$CAPTURE_FILE` and prints stub output.

**Do NOT touch:**
- `.doom.d/projects.el` (per user instruction)

---

## Constraints & Notes

- The user runs a long-lived Emacs daemon. Adding `projects-cmux.el` to the running daemon **does not switch modes** — `MY_PROJECTS_MODE` is read once at config load. Mode switching requires daemon restart. The plan does not request live-reload of cmux mode into the wezterm daemon; it only verifies that the wezterm daemon stays unaffected.
- `tools/emacs` must remain backward compatible: running it from any other terminal must keep working as today.
- ERT tests run via `/opt/homebrew/bin/emacs --batch` per `tools/CLAUDE.md`. Do not use the `emacs` wrapper for tests.
- Each task ends in a commit. Commit messages use Conventional Commits.
- Set `export GPG_TTY=$(tty); gpg-agent --daemon 2>/dev/null || true` before any commit if signing fails.

---

### Task 1: Wrapper test harness

**Files:**
- Create: `tests/fixtures/capture-emacs.sh`
- Create: `tests/fixtures/capture-emacsclient.sh`
- Create: `tests/emacs-wrapper-test.sh`

- [ ] **Step 1: Write capture-emacs.sh fixture**

```sh
#!/bin/sh
# Fake emacs binary. Records argv as a single tab-separated line in $CAPTURE_FILE.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
printf 'emacs'
for arg in "$@"; do
  printf '\t%s' "$arg"
done
printf '\n' >> "$CAPTURE_FILE"
exit 0
```

Make executable: `chmod +x tests/fixtures/capture-emacs.sh`.

- [ ] **Step 2: Write capture-emacsclient.sh fixture**

```sh
#!/bin/sh
# Fake emacsclient. Records argv and exits with the value of $EMACSCLIENT_EXIT
# (default 0). Tests set EMACSCLIENT_EXIT=1 to force the wrapper into the
# fallback "start daemon then connect" branch.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
printf 'emacsclient'
for arg in "$@"; do
  printf '\t%s' "$arg"
done
printf '\n' >> "$CAPTURE_FILE"
exit "${EMACSCLIENT_EXIT:-0}"
```

Make executable.

- [ ] **Step 3: Write the test runner skeleton**

```sh
#!/usr/bin/env bash
# tests/emacs-wrapper-test.sh — drives tools/emacs with mock binaries.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
WRAPPER="$REPO_ROOT/emacs"
FIXTURES="$REPO_ROOT/tests/fixtures"

fail=0
pass=0

run_case() {
  local name="$1"; shift
  local expected_pattern="$1"; shift
  local capture; capture="$(mktemp)"
  (
    export EMACS_BIN="$FIXTURES/capture-emacs.sh"
    export EMACSCLIENT_BIN="$FIXTURES/capture-emacsclient.sh"
    export CAPTURE_FILE="$capture"
    "$@"
  ) >/dev/null 2>&1 || true
  if grep -qE "$expected_pattern" "$capture"; then
    pass=$((pass+1)); printf 'PASS %s\n' "$name"
  else
    fail=$((fail+1))
    printf 'FAIL %s\n  expected match: %s\n  got:\n' "$name" "$expected_pattern"
    sed 's/^/    /' "$capture"
  fi
  rm -f "$capture"
}

# Cases will be added in later tasks.

if [ "$fail" -gt 0 ]; then
  printf '%d failed, %d passed\n' "$fail" "$pass"
  exit 1
fi
printf 'all %d passed\n' "$pass"
```

Make executable.

- [ ] **Step 4: Run the test runner once to verify it executes**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `all 0 passed`

- [ ] **Step 5: Commit**

```bash
git add tests/emacs-wrapper-test.sh tests/fixtures/capture-emacs.sh tests/fixtures/capture-emacsclient.sh
git commit -m "test(emacs-wrapper): add shell test harness with capture fixtures"
```

---

### Task 2: Wrapper binary injection

Refactor `tools/emacs` to honor `EMACS_BIN` / `EMACSCLIENT_BIN` env overrides while preserving its current behavior.

**Files:**
- Modify: `tools/emacs:1-44` (replace the binary-resolution prelude)
- Modify: `tests/emacs-wrapper-test.sh` (add baseline cases)

- [ ] **Step 1: Add baseline failing test cases to tests/emacs-wrapper-test.sh**

Append before the final `if [ "$fail" -gt 0 ]` block:

```sh
# Baseline: with EMACS_BIN set, daemon-start path uses our fake emacs.
run_case 'baseline emacs --daemon used' \
  '^emacs(\t--daemon)?$' \
  env EMACSCLIENT_EXIT=1 "$WRAPPER" -nc --version

# Baseline: emacsclient is invoked when not -nc.
run_case 'baseline emacsclient invoked' \
  '^emacsclient' \
  env EMACSCLIENT_EXIT=0 "$WRAPPER" --version
```

- [ ] **Step 2: Run tests, expect failure**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: both cases FAIL because the wrapper still hardcodes `/opt/homebrew/bin/emacs`.

- [ ] **Step 3: Refactor tools/emacs binary resolution**

Replace lines 1–10 of `tools/emacs` with:

```sh
#!/bin/sh

resolve_bin() {
  case "$1" in
    /*) printf '%s\n' "$1"; return 0 ;;
  esac
  for prefix in /opt/homebrew/bin /usr/local/bin /bin; do
    if [ -x "$prefix/$1" ]; then
      printf '%s/%s\n' "$prefix" "$1"
      return 0
    fi
  done
  printf '%s\n' "$1"
}

emacs_bin="${EMACS_BIN:-$(resolve_bin emacs)}"
emacsclient_bin="${EMACSCLIENT_BIN:-$(resolve_bin emacsclient)}"
```

Replace every later use of `$bin/emacs` with `"$emacs_bin"` and `$bin/emacsclient` with `"$emacsclient_bin"`. Keep all other logic (option parsing, `gui_mode`, `no_client`, `-kill`) intact for now.

- [ ] **Step 4: Run tests, expect pass**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `all 2 passed`

- [ ] **Step 5: Commit**

```bash
git add tools/emacs tests/emacs-wrapper-test.sh
git commit -m "refactor(emacs-wrapper): allow EMACS_BIN/EMACSCLIENT_BIN injection"
```

Note: the wrapper file path is `emacs` at the repo root (the existing `tools/emacs` script). All references in this plan to `tools/emacs` mean that file.

---

### Task 3: Mode detection — cmux vs wezterm vs default

The wrapper detects the surrounding terminal once and exports `MY_PROJECTS_MODE` and `EMACS_DAEMON_NAME`. Detection rules:
- `CMUX_SOCKET_PATH` set → `cmux`.
- `WEZTERM_PANE` set → `wezterm`.
- Otherwise → `default` (and `MY_PROJECTS_MODE=wezterm` so existing behavior is preserved unless explicitly overridden).

A pre-existing `MY_PROJECTS_MODE` or `EMACS_DAEMON_NAME` in the environment wins over auto-detection.

**Files:**
- Modify: `tools/emacs`
- Modify: `tests/emacs-wrapper-test.sh`

- [ ] **Step 1: Add failing test cases**

Append to `tests/emacs-wrapper-test.sh`:

```sh
run_case 'cmux env selects cmux daemon' \
  '^emacs(\t--daemon=cmux)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'cmux env exports MY_PROJECTS_MODE=cmux to client' \
  $'^emacsclient(\t.+)?\t-s\tcmux(\t|$)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=0 "$WRAPPER" --version

run_case 'wezterm env selects wezterm daemon' \
  '^emacs\t--daemon=wezterm' \
  env -u CMUX_SOCKET_PATH WEZTERM_PANE=2 EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'no env selects default daemon' \
  '^emacs\t--daemon=default' \
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACSCLIENT_EXIT=1 "$WRAPPER" --version

run_case 'env override wins' \
  '^emacs\t--daemon=cmux' \
  env -u CMUX_SOCKET_PATH -u WEZTERM_PANE EMACS_DAEMON_NAME=cmux MY_PROJECTS_MODE=cmux EMACSCLIENT_EXIT=1 "$WRAPPER" --version
```

- [ ] **Step 2: Run tests, expect failure**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: the four new cases FAIL.

- [ ] **Step 3: Add detection block to tools/emacs**

Insert after the binary resolution block from Task 2 and before the `no_client=0` line:

```sh
detect_mode() {
  if [ -n "${MY_PROJECTS_MODE:-}" ] && [ -n "${EMACS_DAEMON_NAME:-}" ]; then
    return 0
  fi
  if [ -n "${CMUX_SOCKET_PATH:-}" ]; then
    : "${MY_PROJECTS_MODE:=cmux}"
    : "${EMACS_DAEMON_NAME:=cmux}"
  elif [ -n "${WEZTERM_PANE:-}" ]; then
    : "${MY_PROJECTS_MODE:=wezterm}"
    : "${EMACS_DAEMON_NAME:=wezterm}"
  else
    : "${MY_PROJECTS_MODE:=wezterm}"
    : "${EMACS_DAEMON_NAME:=default}"
  fi
  export MY_PROJECTS_MODE EMACS_DAEMON_NAME
}

detect_mode
```

- [ ] **Step 4: Run tests, expect new failures (commands now wrong)**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `MY_PROJECTS_MODE` is now exported, but the wrapper still calls `emacs --daemon` and `emacsclient -c ...` without using the daemon name. Tests should still fail. This is the intended halfway state.

- [ ] **Step 5: Wire daemon name into emacs/emacsclient invocations**

Replace the final `if [ $no_client -eq 1 ]` block with:

```sh
if [ "$no_client" -eq 1 ]; then
  "$emacs_bin" $nw_flag "$@"
else
  "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag "$@" 2>/dev/null || {
    "$emacs_bin" --daemon="$EMACS_DAEMON_NAME"
    "$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -c $nw_flag "$@"
  }
fi
```

- [ ] **Step 6: Run tests, expect pass**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `all 7 passed`

- [ ] **Step 7: Commit**

```bash
git add tools/emacs tests/emacs-wrapper-test.sh
git commit -m "feat(emacs-wrapper): detect cmux/wezterm and use named daemons"
```

---

### Task 4: Scoped `-kill`

`emacs -kill` should kill only the named daemon for the current environment, not whatever daemon owns the default socket.

**Files:**
- Modify: `tools/emacs`
- Modify: `tests/emacs-wrapper-test.sh`

- [ ] **Step 1: Add failing test case**

Append to `tests/emacs-wrapper-test.sh`:

```sh
run_case '-kill targets named daemon' \
  $'^emacsclient\t-s\tcmux\t-e\t\\(kill-emacs\\)' \
  env CMUX_SOCKET_PATH=/tmp/cmux.sock EMACSCLIENT_EXIT=0 "$WRAPPER" -kill
```

- [ ] **Step 2: Run tests, expect failure**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: the `-kill` case FAILS — it currently invokes `emacsclient -e '(kill-emacs)'` without `-s`.

- [ ] **Step 3: Update -kill branch in tools/emacs**

Find the `-kill)` case in the option-parsing loop and change its body to:

```sh
"$emacsclient_bin" -s "$EMACS_DAEMON_NAME" -e '(kill-emacs)' 2>/dev/null \
  && echo "Emacs daemon '$EMACS_DAEMON_NAME' killed." \
  || echo "No daemon '$EMACS_DAEMON_NAME' running."
exit 0
```

The `detect_mode` call must run before this branch executes, so move `detect_mode` above the option-parsing loop if not already there.

- [ ] **Step 4: Run tests, expect pass**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `all 8 passed`

- [ ] **Step 5: Commit**

```bash
git add tools/emacs tests/emacs-wrapper-test.sh
git commit -m "feat(emacs-wrapper): scope -kill to the active named daemon"
```

---

### Task 5: Backend dispatcher in config.el

**Files:**
- Modify: `.doom.d/config.el` (insert dispatcher near the top, before any `(load! "projects" …)` style call if present, or at the end of the bare-config preamble if `projects.el` is loaded automatically by Doom's autoload).

Determine the right insertion point first:

```bash
grep -n "load! \"projects" .doom.d/config.el || echo "no explicit load!; projects.el is autoloaded"
```

If `projects.el` is autoloaded, the dispatcher must run **before** `after!` blocks reference its functions. The simplest correct placement: the very top of `config.el`, before any other `defvar` or `after!`.

- [ ] **Step 1: Add my/projects-mode defvar and dispatcher**

Insert at the very top of `.doom.d/config.el` (after the `;;; -*- lexical-binding: t; -*-` header if present, otherwise as the first form):

```elisp
(defvar my/projects-mode
  (pcase (getenv "MY_PROJECTS_MODE")
    ("cmux" 'cmux)
    ("wezterm" 'wezterm)
    (_ 'wezterm))
  "Active projects backend (set once at daemon startup from MY_PROJECTS_MODE).")

(when (eq my/projects-mode 'cmux)
  (load! "projects-cmux"))
```

The wezterm path keeps loading via Doom's autoload as it does today; only cmux mode loads `projects-cmux.el` explicitly.

- [ ] **Step 2: Sanity-check syntax**

```bash
/opt/homebrew/bin/emacs --batch \
  --eval '(check-parens)' \
  -l .doom.d/config.el 2>&1 | tail -5 || true
```

Expected: no `Mismatched parentheses` errors. Other errors (missing libraries) are fine in batch mode and can be ignored.

- [ ] **Step 3: Commit**

```bash
git add .doom.d/config.el
git commit -m "feat(doom): dispatch projects backend on MY_PROJECTS_MODE"
```

The actual `projects-cmux.el` file is created in the next tasks; until then, cmux-mode daemons will fail to load. That is acceptable because no cmux-mode daemon exists yet (no wrapper user has `MY_PROJECTS_MODE=cmux` exported by default unless inside cmux, and they will not start a cmux Emacs daemon before `projects-cmux.el` exists).

---

### Task 6: projects-cmux.el skeleton — model state and accessors

This task creates the file with pure model code copied verbatim from `projects.el` and the cmux CLI helper. No cmux operations yet.

**Files:**
- Create: `.doom.d/projects-cmux.el`
- Create: `.doom.d/test-projects-cmux.el`
- Create: `tests/fixtures/cmux-mock.sh`

- [ ] **Step 1: Create cmux-mock.sh fixture**

```sh
#!/bin/sh
# Fake cmux. Records argv to $CAPTURE_FILE and prints a stub line to stdout.
: "${CAPTURE_FILE:?CAPTURE_FILE must be set}"
printf 'cmux'
for arg in "$@"; do
  printf '\t%s' "$arg"
done
printf '\n' >> "$CAPTURE_FILE"
case "$1" in
  ping) exit 0 ;;
  list-workspaces) printf 'workspace:1\tname=foo\n' ;;
  current-workspace) printf 'workspace:1\tname=foo\n' ;;
esac
exit 0
```

Make executable.

- [ ] **Step 2: Write failing skeleton test**

Create `.doom.d/test-projects-cmux.el`:

```elisp
;;; test-projects-cmux.el --- ERT tests for projects-cmux  -*- lexical-binding: t; -*-
(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "projects-cmux.el" dir) nil t))

(ert-deftest projects-cmux/loads-cleanly ()
  (should (boundp 'projects--table))
  (should (hash-table-p projects--table))
  (should (fboundp 'projects-current))
  (should (fboundp 'projects-names))
  (should (fboundp 'projects-cmux--call)))

(provide 'test-projects-cmux)
;;; test-projects-cmux.el ends here
```

- [ ] **Step 3: Run tests, expect file-not-found failure**

```bash
/opt/homebrew/bin/emacs --batch \
  -l ert \
  -l .doom.d/test-projects-cmux.el \
  -f ert-run-tests-batch-and-exit
```

Expected: error loading `projects-cmux.el` (file does not exist).

- [ ] **Step 4: Create projects-cmux.el with copied model**

Create `.doom.d/projects-cmux.el`:

```elisp
;;; projects-cmux.el --- Cmux-flavored projects backend  -*- lexical-binding: t; -*-
;;
;; Self-contained backend selected when MY_PROJECTS_MODE=cmux. Does NOT
;; (require 'projects). Copies the pure model code from projects.el and
;; replaces UI operations with cmux CLI calls.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconst projects--save-file
  (expand-file-name "projects/session.el"
                    (or (bound-and-true-p doom-data-dir) user-emacs-directory))
  "File where project state is persisted.")

(defconst projects--special-buffer-patterns
  '("^\\*Messages\\*$"
    "^\\*scratch\\*$"
    "^\\*Help\\*"
    "^\\*Warnings\\*$"
    "^\\*Compile-Log\\*$"
    "^\\*lsp"
    "^\\*doom"
    "^COMMIT_EDITMSG$"
    "^ \\*Minibuf")
  "Buffers matching these patterns are global (visible in all projects).")

;;; ---------------------------------------------------------------------------
;;; State
;;; ---------------------------------------------------------------------------

(defvar projects--table (make-hash-table :test #'equal)
  "Hash table mapping project name to plist (:dir :buffers :files :switch-time).")

(defvar projects--current nil
  "Name of the currently active project, or nil.")

(defvar-local projects--buffer-project nil
  "Buffer-local: the project this buffer belongs to.")

;;; ---------------------------------------------------------------------------
;;; Diagnostics
;;; ---------------------------------------------------------------------------

(defvar projects-debug t
  "When non-nil, log diagnostic events as \"[projects-cmux] ...\".")

(defun projects--log (fmt &rest args)
  (when projects-debug
    (apply #'message (concat "[projects-cmux] " fmt) args)))

;;; ---------------------------------------------------------------------------
;;; Accessors (pure — copied from projects.el verbatim except as noted)
;;; ---------------------------------------------------------------------------

(defun projects-current (&optional frame)
  (or (frame-parameter (or frame (selected-frame)) 'projects-project)
      projects--current))

(defun projects-get (name)
  (gethash name projects--table))

(defun projects-dir (name)
  (plist-get (gethash name projects--table) :dir))

(defun projects-buffers (name)
  (cl-remove-if-not #'buffer-live-p
                    (plist-get (gethash name projects--table) :buffers)))

(defun projects-names ()
  (hash-table-keys projects--table))

(defun projects-names-mru ()
  (cl-sort (copy-sequence (projects-names))
           (lambda (a b)
             (let ((ta (or (plist-get (gethash a projects--table) :switch-time) 0))
                   (tb (or (plist-get (gethash b projects--table) :switch-time) 0)))
               (> ta tb)))))

(defun projects-hidden-p (name)
  (plist-get (gethash name projects--table) :hidden))

(defun projects-names-visible ()
  (cl-remove-if #'projects-hidden-p (projects-names-mru)))

(defun projects-special-buffer-p (buf)
  (let ((name (buffer-name buf)))
    (cl-some (lambda (pattern) (string-match-p pattern name))
             projects--special-buffer-patterns)))

(defun projects--unique-project-name (base)
  (if (not (gethash base projects--table))
      base
    (let ((n 1))
      (while (gethash (format "%s [%d]" base n) projects--table)
        (setq n (1+ n)))
      (format "%s [%d]" base n))))

;;; ---------------------------------------------------------------------------
;;; Cmux CLI helper
;;; ---------------------------------------------------------------------------

(defvar projects-cmux--cmux-command "cmux"
  "Path to the cmux CLI. Overridable for tests.")

(defun projects-cmux--call (&rest args)
  "Run cmux with ARGS. Return exit code; log stderr/stdout to *projects-cmux*."
  (let ((buf (get-buffer-create "*projects-cmux*")))
    (with-current-buffer buf (goto-char (point-max)))
    (let ((exit (apply #'call-process projects-cmux--cmux-command
                       nil (list buf t) nil args)))
      (unless (zerop exit)
        (message "[projects-cmux] cmux %s → exit %d (see *projects-cmux*)"
                 (mapconcat #'identity args " ") exit))
      exit)))

(provide 'projects-cmux)
;;; projects-cmux.el ends here
```

- [ ] **Step 5: Run tests, expect pass**

```bash
/opt/homebrew/bin/emacs --batch \
  -l ert \
  -l .doom.d/test-projects-cmux.el \
  -f ert-run-tests-batch-and-exit
```

Expected: `Ran 1 tests, 1 results as expected`.

- [ ] **Step 6: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el tests/fixtures/cmux-mock.sh
git commit -m "feat(projects-cmux): add backend skeleton with copied model and cmux helper"
```

---

### Task 7: Frame-to-project init hook

`emacsclient -t -F '((projects-project . "X"))'` creates a frame with a `projects-project` parameter. A hook reads it and binds the frame.

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing test**

Append to `.doom.d/test-projects-cmux.el`:

```elisp
(ert-deftest projects-cmux/frame-init-binds-frame ()
  "Frames created with projects-project parameter switch to that project."
  (puthash "alpha" (list :dir "/tmp/alpha/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (unwind-protect
      (let ((frame (make-frame `((projects-project . "alpha")
                                 (window-system . nil)
                                 (visibility . nil)))))
        (unwind-protect
            (progn
              (projects-cmux--frame-init frame)
              (should (equal (frame-parameter frame 'projects-project) "alpha"))
              (should (equal projects--current "alpha")))
          (delete-frame frame)))
    (remhash "alpha" projects--table)
    (setq projects--current nil)))
```

- [ ] **Step 2: Run tests, expect failure**

```bash
/opt/homebrew/bin/emacs --batch \
  -l ert \
  -l .doom.d/test-projects-cmux.el \
  -f ert-run-tests-batch-and-exit
```

Expected: failure — `projects-cmux--frame-init` is undefined.

- [ ] **Step 3: Implement frame init**

Insert into `.doom.d/projects-cmux.el` immediately after the cmux helper block:

```elisp
;;; ---------------------------------------------------------------------------
;;; Frame-to-project binding
;;; ---------------------------------------------------------------------------

(defun projects-cmux--frame-init (frame)
  "Bind FRAME to its `projects-project' parameter, if any."
  (when-let ((proj (frame-parameter frame 'projects-project)))
    (when (gethash proj projects--table)
      (with-selected-frame frame
        (projects-switch proj)))))

(add-hook 'after-make-frame-functions #'projects-cmux--frame-init)
```

`projects-switch` is added in Task 9; the test will only invoke `projects-cmux--frame-init` after that task. Add a temporary stub now to keep the test for this task green:

```elisp
;; Temporary stub — replaced in Task 9.
(unless (fboundp 'projects-switch)
  (defun projects-switch (name &optional _norecord)
    (set-frame-parameter nil 'projects-project name)
    (setq projects--current name)))
```

- [ ] **Step 4: Run tests, expect pass**

```bash
/opt/homebrew/bin/emacs --batch \
  -l ert \
  -l .doom.d/test-projects-cmux.el \
  -f ert-run-tests-batch-and-exit
```

Expected: `Ran 2 tests, 2 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): bind frames to projects via after-make-frame-functions"
```

---

### Task 8: projects-create

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing test**

Append:

```elisp
(ert-deftest projects-cmux/create-calls-cmux-and-records ()
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (projects-create "beta" "/tmp/beta/")
          (should (gethash "beta" projects--table))
          (should (equal (projects-dir "beta") "/tmp/beta/"))
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((line (buffer-string)))
              (should (string-match-p "new-workspace" line))
              (should (string-match-p "--name\tbeta" line))
              (should (string-match-p "--cwd\t/tmp/beta/" line))
              (should (string-match-p "--command\t" line))
              (should (string-match-p "emacsclient" line))
              (should (string-match-p "projects-project . \"beta\"" line)))))
      (remhash "beta" projects--table)
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

```bash
/opt/homebrew/bin/emacs --batch -l ert -l .doom.d/test-projects-cmux.el -f ert-run-tests-batch-and-exit
```

Expected: `projects-create` is undefined.

- [ ] **Step 3: Implement projects-create**

Append after the frame-init block (and remove the temporary `projects-switch` stub from Task 7 — it is replaced in the next task; for now keep it):

```elisp
;;; ---------------------------------------------------------------------------
;;; CRUD
;;; ---------------------------------------------------------------------------

(defun projects-cmux--emacsclient-command (project)
  "Return the shell command used to attach an emacsclient frame for PROJECT."
  (format "emacsclient -s %s -t -F '((projects-project . %S))'"
          (or (getenv "EMACS_DAEMON_NAME") "cmux")
          project))

(defun projects-create (name dir)
  "Create project NAME at DIR. Mirrors to a cmux workspace."
  (interactive
   (let* ((dir (expand-file-name
                (read-directory-name "Project directory: " nil nil nil)))
          (default-name (projects--unique-project-name
                         (file-name-nondirectory (directory-file-name dir))))
          (name (read-string "Project name: " default-name)))
     (list name dir)))
  (when (gethash name projects--table)
    (user-error "Project '%s' already exists" name))
  (unless (file-directory-p dir)
    (if (y-or-n-p (format "Directory '%s' does not exist. Create it? " dir))
        (make-directory dir t)
      (user-error "Aborted")))
  (let ((dir (file-name-as-directory (expand-file-name dir))))
    (puthash name (list :dir dir :buffers nil :files nil :switch-time 0)
             projects--table)
    (projects--log "create: %s dir=%s" name dir)
    (projects-cmux--call "new-workspace"
                         "--name" name
                         "--cwd" dir
                         "--command" (projects-cmux--emacsclient-command name))
    name))
```

- [ ] **Step 4: Run tests, expect pass**

```bash
/opt/homebrew/bin/emacs --batch -l ert -l .doom.d/test-projects-cmux.el -f ert-run-tests-batch-and-exit
```

Expected: `Ran 3 tests, 3 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): projects-create mirrors to cmux new-workspace"
```

---

### Task 9: projects-switch (frame-local) and projects-cmux-select-workspace

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing tests**

Append:

```elisp
(ert-deftest projects-cmux/switch-is-frame-local ()
  "projects-switch updates the frame parameter and projects--current
without invoking cmux."
  (puthash "gamma" (list :dir "/tmp/gamma/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (projects-switch "gamma")
          (should (equal (frame-parameter nil 'projects-project) "gamma"))
          (should (equal projects--current "gamma"))
          ;; cmux must NOT have been called.
          (should (= 0 (nth 7 (file-attributes capture)))))
      (remhash "gamma" projects--table)
      (setq projects--current nil)
      (set-frame-parameter nil 'projects-project nil)
      (delete-file capture))))

(ert-deftest projects-cmux/select-workspace-calls-cmux ()
  (puthash "delta" (list :dir "/tmp/delta/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (projects-cmux-select-workspace "delta")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((line (buffer-string)))
              (should (string-match-p "select-workspace" line))
              (should (string-match-p "--workspace\tdelta" line)))))
      (remhash "delta" projects--table)
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

Expected: stub `projects-switch` is too permissive (does not call hooks); `projects-cmux-select-workspace` undefined.

- [ ] **Step 3: Replace stub with real implementations**

Remove the temporary stub from Task 7 and add (replace it in the same location):

```elisp
;;; ---------------------------------------------------------------------------
;;; Switching (frame-local)
;;; ---------------------------------------------------------------------------

(defvar projects-switch-hook nil
  "Hook run after switching the current frame's project.")

(defun projects-switch (name &optional norecord)
  "Switch the SELECTED FRAME to project NAME (frame-local). Does not call cmux."
  (interactive
   (list (completing-read "Switch frame to project: "
                          (projects-names-visible) nil t)))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (set-frame-parameter nil 'projects-project name)
  (setq projects--current name)
  (unless norecord
    (let ((proj (gethash name projects--table)))
      (plist-put proj :switch-time (float-time))
      (puthash name proj projects--table)))
  (when-let ((dir (projects-dir name)))
    (setq-default default-directory dir))
  (projects--log "switch (frame-local): %s" name)
  (run-hooks 'projects-switch-hook))

(defun projects-cmux-select-workspace (name)
  "Select cmux workspace NAME. Does not change any frame's project parameter."
  (interactive
   (list (completing-read "Select cmux workspace: "
                          (projects-names-visible) nil t)))
  (projects-cmux--call "select-workspace" "--workspace" name))
```

- [ ] **Step 4: Run tests, expect pass**

Expected: `Ran 5 tests, 5 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): frame-local projects-switch and select-workspace"
```

---

### Task 10: projects-delete and projects-rename

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing tests**

Append:

```elisp
(ert-deftest projects-cmux/delete-calls-cmux-and-removes ()
  (puthash "epsilon" (list :dir "/tmp/epsilon/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (projects-delete "epsilon"))
          (should-not (gethash "epsilon" projects--table))
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((line (buffer-string)))
              (should (string-match-p "close-workspace" line))
              (should (string-match-p "--workspace\tepsilon" line)))))
      (remhash "epsilon" projects--table)
      (delete-file capture))))

(ert-deftest projects-cmux/rename-rewrites-frames ()
  (puthash "zeta" (list :dir "/tmp/zeta/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (let ((frame (make-frame `((projects-project . "zeta")
                                   (window-system . nil)
                                   (visibility . nil)))))
          (unwind-protect
              (progn
                (projects-rename "zeta" "eta")
                (should (gethash "eta" projects--table))
                (should-not (gethash "zeta" projects--table))
                (should (equal (frame-parameter frame 'projects-project) "eta"))
                (with-temp-buffer
                  (insert-file-contents capture)
                  (should (string-match-p "rename-workspace" (buffer-string)))))
            (delete-frame frame)))
      (remhash "zeta" projects--table)
      (remhash "eta" projects--table)
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

- [ ] **Step 3: Implement projects-delete and projects-rename**

Append:

```elisp
(defun projects-delete (name)
  "Delete project NAME. Closes its cmux workspace and kills its buffers."
  (interactive
   (list (completing-read "Delete project: " (projects-names) nil t
                          nil nil (projects-current))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (when (yes-or-no-p (format "Delete project '%s' and kill its buffers? " name))
    (projects-cmux--call "close-workspace" "--workspace" name)
    (dolist (buf (projects-buffers name))
      (kill-buffer buf))
    (remhash name projects--table)
    (when (equal projects--current name)
      (setq projects--current (car (projects-names-visible))))
    (projects--log "delete: %s" name)))

(defun projects-rename (old-name new-name)
  "Rename project OLD-NAME to NEW-NAME and the matching cmux workspace."
  (interactive
   (let ((old (completing-read "Rename project: " (projects-names) nil t
                               nil nil (projects-current))))
     (list old (read-string (format "Rename '%s' to: " old)))))
  (unless (gethash old-name projects--table)
    (user-error "Project '%s' does not exist" old-name))
  (when (gethash new-name projects--table)
    (user-error "Project '%s' already exists" new-name))
  (puthash new-name (gethash old-name projects--table) projects--table)
  (remhash old-name projects--table)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (equal projects--buffer-project old-name)
        (setq-local projects--buffer-project new-name))))
  (dolist (f (frame-list))
    (when (equal (frame-parameter f 'projects-project) old-name)
      (set-frame-parameter f 'projects-project new-name)))
  (when (equal projects--current old-name)
    (setq projects--current new-name))
  (projects-cmux--call "rename-workspace" "--workspace" old-name new-name)
  (projects--log "rename: %s -> %s" old-name new-name))
```

- [ ] **Step 4: Run tests, expect pass**

Expected: `Ran 7 tests, 7 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): projects-delete and projects-rename mirror cmux"
```

---

### Task 11: projects-set-layout via cmux splits

Each new pane gets an `emacsclient` frame initially bound to **the workspace's project** (the current project), per the spec.

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing test**

Append:

```elisp
(ert-deftest projects-cmux/set-layout-2x2-orchestrates-splits ()
  (puthash "theta" (list :dir "/tmp/theta/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name)))
         (projects--current "theta"))
    (unwind-protect
        (progn
          (projects-set-layout "2x2")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              ;; Three splits should have been created.
              (should (>= (cl-count ?\n (split-string s "new-split" t)) 3))
              ;; Three send commands carrying emacsclient should have been issued.
              (let ((sends 0)
                    (start 0))
                (while (string-match "send" s start)
                  (setq sends (1+ sends)
                        start (match-end 0)))
                (should (= sends 3)))
              (should (string-match-p "emacsclient" s))
              (should (string-match-p "projects-project . \"theta\"" s)))))
      (remhash "theta" projects--table)
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

- [ ] **Step 3: Implement projects-set-layout**

Append:

```elisp
;;; ---------------------------------------------------------------------------
;;; Layout
;;; ---------------------------------------------------------------------------

(defconst projects--multi-layouts '("1x1" "2x1" "2x2" "3x2"))

(defun projects-cmux--split (direction)
  "Split current pane in DIRECTION (\"left\"/\"right\"/\"up\"/\"down\")."
  (projects-cmux--call "new-split" direction))

(defun projects-cmux--send-emacsclient (project)
  "Send the project-bound emacsclient command to the active surface."
  (projects-cmux--call "send"
                       (concat (projects-cmux--emacsclient-command project) "\n")))

(defun projects-set-layout (layout)
  "Apply LAYOUT (one of `projects--multi-layouts') in the current cmux workspace.
Each new pane receives an emacsclient frame initially bound to the workspace
project. Existing layout panes beyond the primary one are NOT auto-closed in
this initial implementation."
  (interactive (list (completing-read "Layout: " projects--multi-layouts nil t)))
  (let ((proj (projects-current)))
    (unless proj (user-error "No active project"))
    (pcase layout
      ("1x1" nil)
      ("2x1" (projects-cmux--split "right")
             (projects-cmux--send-emacsclient proj))
      ("2x2" (projects-cmux--split "right")
             (projects-cmux--send-emacsclient proj)
             (projects-cmux--split "down")
             (projects-cmux--send-emacsclient proj)
             (projects-cmux--split "down")
             (projects-cmux--send-emacsclient proj))
      ("3x2" (dotimes (_ 5)
               (projects-cmux--split "right")
               (projects-cmux--send-emacsclient proj)))
      (_ (user-error "Unknown layout: %s" layout)))))
```

(The cmux CLI's `--surface` selector for `send` is left implicit — `cmux send` without `--surface` targets the current surface, which is the most recently created pane. The initial implementation relies on that. A follow-up can capture pane refs from `new-split`'s output for explicit targeting.)

- [ ] **Step 4: Run tests, expect pass**

Expected: `Ran 8 tests, 8 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): projects-set-layout via cmux splits"
```

---

### Task 12: Browser integration via cmux browser pane

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing test**

Append:

```elisp
(ert-deftest projects-cmux/browse-url-opens-cmux-pane ()
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (projects-cmux--browse-url "https://example.com/x")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              (should (string-match-p "new-pane" s))
              (should (string-match-p "--type\tbrowser" s))
              (should (string-match-p "--url\thttps://example.com/x" s)))))
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

- [ ] **Step 3: Implement browser hookup**

Append:

```elisp
;;; ---------------------------------------------------------------------------
;;; Browser
;;; ---------------------------------------------------------------------------

(defun projects-cmux--browse-url (url &rest _)
  "Open URL in a new cmux browser pane in the current workspace."
  (projects-cmux--call "new-pane" "--type" "browser" "--url" url))

(setq browse-url-browser-function #'projects-cmux--browse-url)
```

- [ ] **Step 4: Run tests, expect pass**

Expected: `Ran 9 tests, 9 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): route browse-url to cmux browser pane"
```

---

### Task 13: projects-cmux-resync

`projects-cmux-resync` walks `projects--table` and creates any missing cmux workspaces. Useful after a cmux restart.

**Files:**
- Modify: `.doom.d/projects-cmux.el`
- Modify: `.doom.d/test-projects-cmux.el`

- [ ] **Step 1: Add failing test**

Append:

```elisp
(ert-deftest projects-cmux/resync-creates-missing-workspaces ()
  (puthash "iota" (list :dir "/tmp/iota/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (puthash "kappa" (list :dir "/tmp/kappa/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       (file-name-directory load-file-name))))
    (unwind-protect
        (progn
          (projects-cmux-resync)
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              (should (string-match-p "new-workspace" s))
              (should (string-match-p "--name\tiota" s))
              (should (string-match-p "--name\tkappa" s)))))
      (remhash "iota" projects--table)
      (remhash "kappa" projects--table)
      (delete-file capture))))
```

- [ ] **Step 2: Run tests, expect failure**

- [ ] **Step 3: Implement projects-cmux-resync**

Append:

```elisp
;;; ---------------------------------------------------------------------------
;;; Resync
;;; ---------------------------------------------------------------------------

(defun projects-cmux-resync ()
  "Create cmux workspaces for every project in `projects--table'.
Idempotent: cmux returns non-zero for an existing workspace and we ignore it."
  (interactive)
  (dolist (name (projects-names))
    (let ((dir (projects-dir name)))
      (projects-cmux--call "new-workspace"
                           "--name" name
                           "--cwd" (or dir "")
                           "--command" (projects-cmux--emacsclient-command name)))))
```

- [ ] **Step 4: Run tests, expect pass**

Expected: `Ran 10 tests, 10 results as expected`.

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects-cmux.el .doom.d/test-projects-cmux.el
git commit -m "feat(projects-cmux): projects-cmux-resync recreates workspaces"
```

---

### Task 14: Smoke verification

Manual verification covering both modes. No new code; just a checklist that must pass before declaring the implementation done.

- [ ] **Step 1: Wezterm path unchanged**

In a wezterm pane (with the existing daemon already running), confirm:

```bash
emacsclient -e '(symbol-value (quote my/projects-mode))'
```

Expected: `wezterm` (or, if the running daemon predates this change, the symbol may be unbound — that is acceptable: the daemon was not restarted).

In a fresh wezterm-spawned daemon (use `tools/emacs --version` to bootstrap), the same form must return `wezterm`.

- [ ] **Step 2: Cmux path bootstraps**

Inside cmux, run:

```bash
tools/emacs -nc --batch -l .doom.d/init.el -l .doom.d/config.el \
  --eval '(message "mode=%s" my/projects-mode)' \
  --eval '(message "cmux-loaded=%s" (featurep (quote projects-cmux)))' \
  --eval '(kill-emacs)'
```

Expected: `mode=cmux` and `cmux-loaded=t` in the messages.

- [ ] **Step 3: Run the full ERT suite**

```bash
/opt/homebrew/bin/emacs --batch -l ert -l .doom.d/test-projects-cmux.el -f ert-run-tests-batch-and-exit
```

Expected: all tests pass.

- [ ] **Step 4: Run the wrapper test suite**

```bash
bash tests/emacs-wrapper-test.sh
```

Expected: `all <N> passed`.

- [ ] **Step 5: Commit (only if any verification fix was needed)**

If verification revealed real issues, file a fix as a follow-up commit. If everything passed, no commit is needed for this task.

---

## Self-review

**Spec coverage:**
- Backend selection — Task 5 ✓
- Emacs daemon selection (named daemons, MY_PROJECTS_MODE) — Tasks 2–4 ✓
- Project model copy — Task 6 ✓
- Frame-to-project binding — Task 7 ✓
- projects-create — Task 8 ✓
- projects-delete — Task 10 ✓
- projects-rename — Task 10 ✓
- projects-switch (frame-local) and projects-cmux-select-workspace — Task 9 ✓
- projects-set-layout — Task 11 ✓
- Browser integration — Task 12 ✓
- Error handling helper — Task 6 (in `projects-cmux--call`) ✓
- projects-cmux-resync — Task 13 ✓
- Startup behavior, no auto-create — implied by absence of any startup workspace creation; no task creates workspaces from `projects--table` at load time. ✓

**Placeholder scan:** every step contains the actual code, command, and expected output. No "TBD" / "TODO" / "fill in" / "appropriate handling" remains.

**Type/name consistency:** `projects-cmux--call`, `projects-cmux--cmux-command`, `projects-cmux--frame-init`, `projects-cmux--emacsclient-command`, `projects-cmux--browse-url`, `projects-cmux-select-workspace`, `projects-cmux-resync`, `projects-cmux--split`, `projects-cmux--send-emacsclient` — used identically across tasks. Public API (`projects-create`, `projects-delete`, `projects-rename`, `projects-switch`, `projects-set-layout`) matches `projects.el`.

**Out-of-scope items deliberately deferred:**
- Pane-ref tracking for explicit `cmux send --surface` targeting (Task 11 relies on implicit "current surface").
- Closing secondary panes when shrinking layout (e.g. 2x2 → 1x1).
- Buffer-registration hooks (`projects--find-file-hook` and friends from `projects.el`) — not copied; cmux-mode does not need them for the initial implementation because every emacsclient frame is created with `projects-project` already set. If buffer-to-project association becomes needed later, copy from `projects.el:560-587` and the hook installation block at `projects.el:1212-1233`.
- Reuse-by-workspace policy for browser panes.
