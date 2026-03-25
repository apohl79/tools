# Clone From Git Implementation Plan

**Goal:** Add `projects-clone-from-git` command (bound to `C-c w g`) that clones a repo into a chosen directory, registers it as a project, and switches to it.
**Type:** Feature
**JIRA:** none
**Tech Stack:** Emacs Lisp (Doom Emacs)
**Code Standards:** n/a
**Executed:** [x]

---

## Context

New interactive command added to the existing custom `projects.el` system. The user flow:
1. Pick a root directory (general `read-directory-name` picker)
2. Enter repo URL — either GitHub shorthand (`org/repo`) or full URL (`https://…`, `git@…`, `git://…`)
3. Confirm project name (defaults to the repo name extracted from the URL)

Clone command selection:
- Full URL (starts with `http`, `https://`, `git@`, `git://`) → `git clone URL TARGET`
- GitHub shorthand (`org/repo` format) → `gh repo clone ORG/REPO TARGET`

After a successful clone the existing `projects-create` is called non-interactively — it handles registration in `projects--table` and calls `projects-switch`.

Edge cases handled:
- Target directory already exists → `user-error`
- `gh` CLI not found when shorthand is used → `user-error`
- Clone process exits non-zero → `user-error` pointing to `*projects-clone*` buffer

No scope creep: no workspace/layout setup beyond what `projects-switch` already does.

## Acceptance Criteria

- [ ] `C-c w g` (and `SPC w g`) invokes the new command
- [ ] `read-directory-name` is used for root dir selection
- [ ] GitHub shorthand (`org/repo`) triggers `gh repo clone`
- [ ] Full URLs (`http*`, `git@*`, `git://*`) trigger `git clone`
- [ ] Project name defaults to the repo name (last path component, `.git` stripped)
- [ ] After successful clone the project is registered and Emacs switches to it
- [ ] Existing directory → clear `user-error` message
- [ ] Missing `gh` for shorthand → clear `user-error` message
- [ ] Clone failure → `user-error` pointing to `*projects-clone*` buffer

---

### Task 1: Add `projects-clone-from-git` to projects.el

**Files:**
- Modify: `.doom.d/projects.el`

**Step 1: Insert function after `projects-create` (after line 142)**

Add the following function immediately after `projects-create` (before the `projects-rename` defun at line 144):

```elisp
(defun projects--repo-name-from-url (url)
  "Extract the repository name from URL or GitHub shorthand ORG/REPO.
Strips a trailing .git suffix if present."
  (if (string-match "/\\([^/]+?\\)\\(\\.git\\)?$" url)
      (match-string 1 url)
    url))

(defun projects-clone-from-git (root-dir repo-url project-name)
  "Clone a git repository and register it as a new project.
ROOT-DIR is the parent directory.  REPO-URL is either an ORG/REPO
GitHub shorthand (cloned via `gh repo clone') or a full URL starting
with http/https/git (cloned via `git clone').  PROJECT-NAME becomes
both the target directory name (ROOT-DIR/PROJECT-NAME) and the project
name registered in `projects--table'."
  (interactive
   (let* ((root (read-directory-name "Clone into directory: " nil nil t))
          (url  (read-string "Repository (org/repo or URL): "))
          (name (read-string "Project name: "
                             (projects--repo-name-from-url url))))
     (list root url name)))
  (let* ((target (expand-file-name project-name root-dir))
         (github-p (not (string-match "^\\(https?://\\|git://\\|git@\\)" repo-url)))
         (cmd  (if github-p
                   (list "gh" "repo" "clone" repo-url target)
                 (list "git" "clone" repo-url target))))
    (when (file-exists-p target)
      (user-error "Directory '%s' already exists" target))
    (when (and github-p (not (executable-find "gh")))
      (user-error "gh CLI not found — install it or use a full URL"))
    (message "[projects] cloning %s into %s..." repo-url target)
    (let ((buf (get-buffer-create "*projects-clone*")))
      (with-current-buffer buf (erase-buffer))
      (let ((exit-code (apply #'call-process (car cmd) nil buf t (cdr cmd))))
        (if (zerop exit-code)
            (progn
              (message "[projects] clone succeeded")
              (projects-create project-name target))
          (user-error "Clone failed (exit %d) — see *projects-clone* buffer"
                      exit-code))))))
```

**Step 2: Verify syntax loads**

Run:
```
doom sync --no-env 2>&1 | tail -5
```
Or inside Emacs: `M-x load-file RET ~/.doom.d/projects.el RET` — should produce no errors.

**Step 3: Commit**

```
feat(projects): add projects-clone-from-git command
```

---

### Task 2: Wire keybinding in config.el

**Depends on:** Task 1

**Files:**
- Modify: `.doom.d/config.el`

**Step 1: Add the `"g"` binding**

Locate the `:leader (:prefix ("w" . "projects") ...)` block (lines 316–328). Insert one new line after line 328 (the `:desc "Project info buffer"` line) and **before** the closing `))`):

Current (lines 326-328):
```elisp
  :desc "Restore from backup…"       "l" (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'projects-restore)))
  :desc "Save projects state"        "a" #'projects-save
  :desc "Project info buffer"        "i" #'projects-show-info)
```

Change to:
```elisp
  :desc "Restore from backup…"       "l" (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'projects-restore)))
  :desc "Save projects state"        "a" #'projects-save
  :desc "Project info buffer"        "i" #'projects-show-info
  :desc "Clone from git"             "g" #'projects-clone-from-git)
```

**Step 2: Verify keybinding registers**

Inside Emacs after `M-x doom/reload`:
- `C-c w g` should invoke `projects-clone-from-git`
- `SPC w g` should also invoke it (`:leader` provides both)
- `C-h k C-c w g` should show the binding description "Clone from git"

**Step 3: Commit**

```
feat(projects): bind C-c w g to projects-clone-from-git
```

---

## Task Dependency Graph

```
Task 1 ──> Task 2
```

## Open Questions

- None
