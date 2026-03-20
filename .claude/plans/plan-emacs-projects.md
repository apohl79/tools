# Emacs Projects System Implementation Plan

**Goal:** Replace Doom's persp-mode workspace system with a custom `projects` system that strictly isolates buffers per directory-bound project, shows all projects in the tab-bar, and auto-saves/restores context with a progress display.
**Type:** Feature
**JIRA:** none
**Tech Stack:** Emacs Lisp, Doom Emacs (non-evil), macOS
**Code Standards:** n/a (Emacs Lisp — follow existing file conventions)
**Executed:** [x]

---

## Context

The user is dissatisfied with Doom Emacs's built-in workspace system (`persp-mode`) because buffers frequently lose workspace context or appear in wrong workspaces. The solution is to build a custom workspace system from scratch called `projects`.

**Key requirements:**
- Each project is bound to a root directory (immutable after creation)
- Buffers created in a project stay there; can only be moved explicitly by the user
- All projects visible in tab-bar (active highlighted, others inactive, MRU-sorted)
- Empty projects show a custom read-only info buffer
- Auto-save on timer (5 min) + on exit; restore on startup with progress display
- Special buffers (`*Messages*`, `*scratch*`, Magit, etc.) are global (visible everywhere)
- Files opened outside the current project's directory are associated with the current project
- ibuffer integration: group buffers by project
- Doom dashboard integration: list projects with directory (replace current persp-mode widget)
- Keybindings: `C-c w` prefix; also replaces Doom's workspace keymap
- **Out of scope:** Remote/TRAMP buffer handling

**Existing code to preserve (reuse):**
- `my/quickload-session` progress display mechanism in `+functions.el:296` → adapt for projects restore
- `tab-bar` faces `my/workspace-tab-active` / `my/workspace-tab-inactive` in `+functions.el:1051`
- Theme-aware face setup in `config.el:123`
- `ibuffer` format config in `config.el:587`
- `my/restore-session-on-first-frame` startup hook pattern

## Acceptance Criteria

- [ ] `workspaces` module removed from `init.el`; `persp-mode` disabled in `packages.el`
- [ ] `projects.el` provides all project CRUD operations (create/rename/delete/switch)
- [ ] New buffers opened via `find-file` are automatically associated with the active project
- [ ] Buffers cannot appear in non-owning projects; `projects-switch-buffer` filters to current project
- [ ] A project with no file buffers shows a read-only info buffer with name/dir/count
- [ ] Tab-bar shows all projects MRU-sorted; active project is highlighted
- [ ] Projects state saved to `~/.config/emacs/.local/etc/projects/session.el` on timer + exit
- [ ] Restore function opens files with animated progress display (reusing `*session-loading*` pattern)
- [ ] ibuffer groups buffers by project
- [ ] Doom dashboard lists projects with path (MRU-sorted, clickable)
- [ ] `C-c w w` switches projects, `C-c w n` creates, `C-c w r` renames, `C-c w k` deletes, `C-c w m` moves buffer, `C-c w b` switches buffer within project
- [ ] All persp-mode code removed from `config.el` and `+functions.el`

---

### Task 1: Disable persp-mode, scaffold projects.el

**Files:**
- Modify: `.doom.d/init.el`
- Modify: `.doom.d/packages.el`
- Create: `.doom.d/projects.el`
- Modify: `.doom.d/config.el`

**Step 1: Remove workspaces module from init.el**

In `.doom.d/init.el`, find and remove the `workspaces` line under `:ui`:
```
;; REMOVE this line:
workspaces          ; tab emulation, persistence & separate workspaces
```

**Step 2: Disable persp-mode package**

In `.doom.d/packages.el`, add after the existing entries:
```elisp
;; Replace Doom's workspace system with our custom projects system
(package! persp-mode :disable t)
```

**Step 3: Create projects.el scaffold**

Create `.doom.d/projects.el` with this exact content:
```elisp
;;; projects.el --- Custom project/workspace system for Doom Emacs -*- lexical-binding: t; -*-
;;
;; Replaces persp-mode with a buffer-strict, directory-bound project system.
;; Each project owns its buffers. Buffers cannot migrate between projects.

;;; Code:

(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconst projects--save-file
  (expand-file-name "projects/session.el" doom-data-dir)
  "File where project state is persisted.")

(defconst projects--special-buffer-patterns
  '("^\\*Messages\\*$"
    "^\\*scratch\\*$"
    "^\\*Help\\*"
    "^\\*Warnings\\*$"
    "^\\*Compile-Log\\*$"
    "^\\*lsp"
    "^magit"
    "^\\*doom"
    "^\\*vterm"
    "^\\*eat"
    "^COMMIT_EDITMSG$")
  "Buffers matching these patterns are global (visible in all projects).")

;;; ---------------------------------------------------------------------------
;;; State
;;; ---------------------------------------------------------------------------

(defvar projects--table (make-hash-table :test #'equal)
  "Hash table mapping project name (string) to project plist.
Each value is a plist: (:dir DIR :buffers BUFFER-LIST :files FILE-LIST :switch-time TIME)")

(defvar projects--current nil
  "Name of the currently active project (string), or nil if none.")

(defvar-local projects--buffer-project nil
  "Buffer-local variable: the name of the project this buffer belongs to.
nil means the buffer is global (special buffer).")

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'projects)
;;; projects.el ends here
```

**Step 4: Load projects.el from config.el**

In `.doom.d/config.el`, add near the top (after the `require` and `setq` for user info):
```elisp
;; Load custom projects system (replaces persp-mode/workspaces)
(load! "projects")
```

**Step 5: Verify**

Run `doom sync` and ensure Emacs starts without persp-mode errors:
```
doom sync
```
Expected: Emacs loads without persp-mode-related errors. The `projects--table` variable is accessible.

**Step 6: Commit**
```
feat(emacs): scaffold projects.el workspace system, disable persp-mode
```

---

### Task 2: Core project data model (CRUD operations)

**Files:**
- Modify: `.doom.d/projects.el`

Add after the `;;; State` section and before `;;; Provide`:

**Step 1: Add accessor helpers**

```elisp
;;; ---------------------------------------------------------------------------
;;; Accessors
;;; ---------------------------------------------------------------------------

(defun projects-current ()
  "Return the name of the active project, or nil."
  projects--current)

(defun projects-get (name)
  "Return the plist for project NAME, or nil."
  (gethash name projects--table))

(defun projects-dir (name)
  "Return the root directory of project NAME."
  (plist-get (gethash name projects--table) :dir))

(defun projects-buffers (name)
  "Return the live buffer list for project NAME."
  (cl-remove-if-not #'buffer-live-p
                    (plist-get (gethash name projects--table) :buffers)))

(defun projects-names ()
  "Return list of all project names."
  (hash-table-keys projects--table))

(defun projects-names-mru ()
  "Return project names sorted by last switch time (most recent first)."
  (let ((names (projects-names)))
    (sort names
          (lambda (a b)
            (let ((ta (or (plist-get (gethash a projects--table) :switch-time) 0))
                  (tb (or (plist-get (gethash b projects--table) :switch-time) 0)))
              (> ta tb))))))

(defun projects-special-buffer-p (buf)
  "Return t if BUF is a global/special buffer that should appear in all projects."
  (let ((name (buffer-name buf)))
    (cl-some (lambda (pattern) (string-match-p pattern name))
             projects--special-buffer-patterns)))
```

**Step 2: Add project CRUD**

```elisp
;;; ---------------------------------------------------------------------------
;;; CRUD
;;; ---------------------------------------------------------------------------

(defun projects-create (name dir)
  "Create a new project named NAME with root directory DIR.
NAME must be unique. DIR must be an existing directory."
  (interactive
   (let* ((name (read-string "Project name: "))
          (dir (read-directory-name "Project directory: " nil nil t)))
     (list name (expand-file-name dir))))
  (when (gethash name projects--table)
    (user-error "Project '%s' already exists" name))
  (unless (file-directory-p dir)
    (user-error "Directory '%s' does not exist" dir))
  (puthash name (list :dir (file-name-as-directory (expand-file-name dir))
                      :buffers nil
                      :files nil
                      :switch-time 0)
           projects--table)
  (message "Project '%s' created → %s" name dir)
  (projects-switch name)
  name)

(defun projects-rename (old-name new-name)
  "Rename project OLD-NAME to NEW-NAME."
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
  ;; Update buffer-local project references
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (equal projects--buffer-project old-name)
        (setq-local projects--buffer-project new-name))))
  ;; Update current if needed
  (when (equal projects--current old-name)
    (setq projects--current new-name))
  (projects--tab-bar-refresh)
  (message "Project renamed: '%s' → '%s'" old-name new-name))

(defun projects-delete (name)
  "Delete project NAME. Buffers belonging to it are killed."
  (interactive
   (list (completing-read "Delete project: " (projects-names) nil t
                          nil nil (projects-current))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (when (yes-or-no-p (format "Delete project '%s' and kill its buffers? " name))
    ;; Kill all project buffers
    (dolist (buf (projects-buffers name))
      (kill-buffer buf))
    (remhash name projects--table)
    ;; Switch to another project if this was active
    (when (equal projects--current name)
      (let ((others (projects-names-mru)))
        (if others
            (projects-switch (car others))
          (setq projects--current nil))))
    (projects--tab-bar-refresh)
    (message "Project '%s' deleted" name)))
```

**Step 3: Add switch function**

```elisp
;;; ---------------------------------------------------------------------------
;;; Switching
;;; ---------------------------------------------------------------------------

(defun projects-switch (name &optional norecord)
  "Switch to project NAME. Updates tab-bar and active buffers."
  (interactive
   (list (completing-read "Switch to project: "
                          (lambda (str pred action)
                            (if (eq action 'metadata)
                                '(metadata (display-sort-function . identity))
                              (complete-with-action action (projects-names-mru) str pred)))
                          nil t)))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (setq projects--current name)
  (unless norecord
    (let ((proj (gethash name projects--table)))
      (plist-put proj :switch-time (float-time))
      (puthash name proj projects--table)))
  ;; Show the project's buffers or the info buffer
  (projects--ensure-visible-buffer)
  (projects--tab-bar-refresh)
  (run-hooks 'projects-switch-hook))

(defvar projects-switch-hook nil
  "Hook run after switching to a project.")
```

**Step 4: Verify**

Load the updated `projects.el` in a running Emacs (`M-x load-file RET ~/.doom.d/projects.el`) and verify:
- `(projects-create "test" "~/")` creates a project entry
- `(projects-names)` returns `("test")`
- `(projects-rename "test" "test2")` renames it
- `(projects-delete "test2")` deletes it

**Step 5: Commit**
```
feat(emacs/projects): add core project data model and CRUD operations
```

---

### Task 3: Buffer registration and tracking

**Depends on:** Task 2

**Files:**
- Modify: `.doom.d/projects.el`

**Step 1: Add buffer registration**

Add before `;;; Provide`:

```elisp
;;; ---------------------------------------------------------------------------
;;; Buffer Management
;;; ---------------------------------------------------------------------------

(defun projects-register-buffer (buf &optional project-name)
  "Register BUF as belonging to PROJECT-NAME (defaults to current project).
Does nothing if BUF is a special/global buffer."
  (let ((proj (or project-name projects--current)))
    (when (and proj (not (projects-special-buffer-p buf)))
      (with-current-buffer buf
        (setq-local projects--buffer-project proj))
      (let* ((entry (gethash proj projects--table))
             (bufs (plist-get entry :buffers)))
        (unless (memq buf bufs)
          (plist-put entry :buffers (cons buf bufs))
          (puthash proj entry projects--table))))))

(defun projects--find-file-hook ()
  "Register newly opened files with the current project."
  (projects-register-buffer (current-buffer)))

(defun projects--cleanup-dead-buffers ()
  "Remove dead buffers from all project buffer lists."
  (maphash
   (lambda (name entry)
     (let ((live (cl-remove-if-not #'buffer-live-p (plist-get entry :buffers))))
       (plist-put entry :buffers live)
       (puthash name entry projects--table)))
   projects--table))

(defun projects-switch-buffer ()
  "Switch to a buffer belonging to the current project."
  (interactive)
  (let* ((proj projects--current)
         (bufs (if proj
                   (cl-remove-if-not
                    (lambda (b)
                      (or (projects-special-buffer-p b)
                          (with-current-buffer b
                            (equal projects--buffer-project proj))))
                    (buffer-list))
                 (buffer-list)))
         (names (mapcar #'buffer-name bufs)))
    (switch-to-buffer
     (completing-read (format "Buffer [%s]: " (or proj "global")) names nil t))))

(defun projects-move-buffer (buffer target-project)
  "Move BUFFER to TARGET-PROJECT."
  (interactive
   (list (current-buffer)
         (completing-read "Move buffer to project: " (projects-names) nil t)))
  (when (projects-special-buffer-p buffer)
    (user-error "Cannot move a global/special buffer to a project"))
  (let ((old-proj (with-current-buffer buffer projects--buffer-project)))
    ;; Remove from old project's buffer list
    (when old-proj
      (let* ((entry (gethash old-proj projects--table))
             (bufs (plist-get entry :buffers)))
        (plist-put entry :buffers (delq buffer bufs))
        (puthash old-proj entry projects--table)))
    ;; Register in target project
    (projects-register-buffer buffer target-project)
    (message "Buffer '%s' moved to project '%s'" (buffer-name buffer) target-project)))
```

**Step 2: Install hooks**

Add after the buffer management section:

```elisp
;;; ---------------------------------------------------------------------------
;;; Setup Hooks
;;; ---------------------------------------------------------------------------

(defun projects--setup-hooks ()
  "Install hooks for buffer tracking."
  (add-hook 'find-file-hook #'projects--find-file-hook)
  (add-hook 'kill-buffer-hook #'projects--cleanup-dead-buffers))
```

Call `(projects--setup-hooks)` near the end of `projects.el` before `(provide 'projects)`.

**Step 3: Verify**

In a running Emacs with projects loaded:
- Create a project: `(projects-create "mytest" "~/")`
- Switch to it: `(projects-switch "mytest")`
- Open a file: the buffer should have `projects--buffer-project` set to `"mytest"`
- Verify: `(with-current-buffer (current-buffer) projects--buffer-project)` → `"mytest"`

**Step 4: Commit**
```
feat(emacs/projects): add buffer registration and tracking hooks
```

---

### Task 4: Project info buffer (empty project state)

**Depends on:** Task 3

**Files:**
- Modify: `.doom.d/projects.el`

**Step 1: Add info buffer functions**

Add before `;;; Setup Hooks`:

```elisp
;;; ---------------------------------------------------------------------------
;;; Info Buffer (empty project placeholder)
;;; ---------------------------------------------------------------------------

(defun projects--info-buffer-name (project-name)
  "Return the info buffer name for PROJECT-NAME."
  (format "*project: %s*" project-name))

(defun projects--create-info-buffer (project-name)
  "Create or refresh the read-only info buffer for PROJECT-NAME.
Returns the buffer."
  (let* ((buf-name (projects--info-buffer-name project-name))
         (buf (get-buffer-create buf-name))
         (entry (gethash project-name projects--table))
         (dir (plist-get entry :dir))
         (buf-count (length (projects-buffers project-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local projects--buffer-project nil)  ; info buffer is global
        (insert "\n\n")
        (insert (propertize (format "  Project: %s\n" project-name)
                            'face '(:weight bold :height 1.4)))
        (insert (propertize (format "  Directory: %s\n" (abbreviate-file-name dir))
                            'face 'font-lock-comment-face))
        (insert (propertize (format "  Buffers: %d\n" buf-count)
                            'face 'font-lock-comment-face))
        (insert "\n")
        (insert (propertize "  This project has no open buffers.\n"
                            'face 'font-lock-doc-face))
        (insert "  Open a file with C-x C-f to get started.\n"))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    buf))

(defun projects--ensure-visible-buffer ()
  "If the current project has no file buffers, show its info buffer."
  (let* ((proj projects--current)
         (bufs (when proj (projects-buffers proj)))
         ;; Filter to non-info, non-special file buffers
         (file-bufs (when bufs
                      (cl-remove-if
                       (lambda (b)
                         (or (projects-special-buffer-p b)
                             (string-match-p "^\\*project: " (buffer-name b))))
                       bufs))))
    (if (and proj (null file-bufs))
        ;; Show info buffer
        (switch-to-buffer (projects--create-info-buffer proj))
      ;; Show most recently used buffer for this project
      (when proj
        (let ((visible (cl-find-if
                        (lambda (b)
                          (with-current-buffer b
                            (equal projects--buffer-project proj)))
                        (buffer-list))))
          (when visible
            (switch-to-buffer visible)))))))
```

**Step 2: Verify**

- Create a project: `(projects-create "empty-test" "~/")`
- Switch to it. A buffer `*project: empty-test*` should appear showing project info.

**Step 3: Commit**
```
feat(emacs/projects): add info buffer for empty projects
```

---

### Task 5: Tab-bar integration

**Depends on:** Task 2

**Files:**
- Modify: `.doom.d/projects.el`
- Modify: `.doom.d/config.el`

**Step 1: Add tab-bar format function to projects.el**

Add before `;;; Setup Hooks`:

```elisp
;;; ---------------------------------------------------------------------------
;;; Tab-bar Integration
;;; ---------------------------------------------------------------------------

(defun projects--tab-bar-format ()
  "Tab-bar format function: renders all projects MRU-sorted with active one highlighted.
Reuses faces my/workspace-tab-active and my/workspace-tab-inactive from +functions.el."
  (let ((current projects--current)
        (names (projects-names-mru)))
    (mapcar
     (lambda (name)
       (let ((face (if (equal name current)
                       'my/workspace-tab-active
                     'my/workspace-tab-inactive))
             (captured-name name))
         `(,(intern (concat "proj-" name))
           menu-item
           ,(propertize (format " %s " name) 'face face)
           (lambda () (interactive) (projects-switch ,captured-name t))
           :help ,(format "Switch to project: %s → %s"
                          name
                          (abbreviate-file-name (or (projects-dir name) ""))))))
     names)))

(defun projects--tab-bar-refresh (&rest _)
  "Force tab-bar redraw after project state changes."
  (when (bound-and-true-p tab-bar-mode)
    (force-mode-line-update t)))
```

**Step 2: Update config.el to use projects tab-bar**

In `.doom.d/config.el`, find the existing `(after! persp-mode ...)` block for tab-bar (around line 96) and **replace the entire block** with:

```elisp
;; Projects tab-bar integration
;; Uses my/workspace-tab-active / my/workspace-tab-inactive faces (defined in +functions.el)
(after! projects
  (setq tab-bar-define-keys nil
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show t
        tab-bar-format '(projects--tab-bar-format))
  (tab-bar-mode 1)
  (add-hook 'projects-switch-hook #'projects--tab-bar-refresh))
```

Also remove the `persp-activated-functions` tab-bar hooks that follow (the `remove-hook` calls).

**Step 3: Keep the theme-aware face setup**

The existing face setup in `config.el` at line ~123 (the `doom-load-theme-hook` block) should be **kept as-is** since it sets `my/workspace-tab-active` and `my/workspace-tab-inactive` which `projects--tab-bar-format` reuses.

**Step 4: Verify**

After `doom sync` + Emacs restart:
- Tab-bar shows project names
- Active project is highlighted in blue
- Clicking a tab calls `projects-switch`

**Step 5: Commit**
```
feat(emacs/projects): add tab-bar integration showing all projects
```

---

### Task 6: Persistence — save and restore with progress

**Depends on:** Tasks 2, 3, 4, 5

**Files:**
- Modify: `.doom.d/projects.el`

**Step 1: Add save function**

Add before `;;; Setup Hooks`:

```elisp
;;; ---------------------------------------------------------------------------
;;; Persistence
;;; ---------------------------------------------------------------------------

(defun projects-save ()
  "Save project state to `projects--save-file'.
Saves project names, directories, switch times, and open file paths."
  (interactive)
  (make-directory (file-name-directory projects--save-file) t)
  (let ((data
         (mapcar
          (lambda (name)
            (let* ((entry (gethash name projects--table))
                   (files (delq nil
                                (mapcar (lambda (b)
                                          (when (buffer-live-p b)
                                            (buffer-file-name b)))
                                        (plist-get entry :buffers)))))
              (list name
                    :dir (plist-get entry :dir)
                    :files files
                    :switch-time (or (plist-get entry :switch-time) 0))))
          (projects-names))))
    (with-temp-file projects--save-file
      (let ((print-level nil)
            (print-length nil))
        (pp (list :version 1
                  :current projects--current
                  :projects data)
            (current-buffer)))))
  (let ((inhibit-message t))
    (message "Projects saved")))
```

**Step 2: Add restore function with progress display**

```elisp
(defun projects-restore ()
  "Restore project state from `projects--save-file'.
Shows an animated progress buffer while reopening files.
Modeled after the existing my/quickload-session pattern."
  (interactive)
  (if (not (file-exists-p projects--save-file))
      (message "No saved projects session found")
    (let* ((data (with-temp-buffer
                   (insert-file-contents projects--save-file)
                   (read (current-buffer))))
           (version (plist-get data :version))
           (saved-current (plist-get data :current))
           (project-list (plist-get data :projects)))
      (if (not (equal version 1))
          (message "Unknown projects session version: %s" version)
        ;; --- valid session: proceed with restore ---
        (let* ((total (apply #'+ (mapcar (lambda (proj)
                                           (length (plist-get (cdr proj) :files)))
                                         project-list)))
               (current-count 0)
               (loading-buffer (get-buffer-create "*session-loading*"))
               (blue-color  (if (fboundp 'doom-color) (doom-color 'blue)  "#4f97d7"))
               (green-color (if (fboundp 'doom-color) (doom-color 'green) "#67b11d")))

          ;; Show progress buffer
          (switch-to-buffer loading-buffer)
          (with-current-buffer loading-buffer
            (erase-buffer)
            (setq buffer-read-only nil)
            (insert "\n\n  ")
            (insert (propertize "Restoring Projects"
                                'face `(:height 1.5 :weight bold :foreground ,blue-color)))
            (insert (format "\n\n  Loading %d file(s) across %d project(s)...\n\n  "
                            total (length project-list)))
            (setq-local progress-start-marker (point-marker))
            (insert (propertize "0% " 'face `(:height 1.2 :foreground ,green-color)))
            (setq-local progress-end-marker (point-marker))
            (setq buffer-read-only t))
          (redisplay t)

          ;; Restore project metadata (clear old state first)
          (clrhash projects--table)
          (dolist (proj-entry project-list)
            (let ((name (car proj-entry))
                  (pdata (cdr proj-entry)))
              (puthash name
                       (list :dir (plist-get pdata :dir)
                             :buffers nil
                             :files (plist-get pdata :files)
                             :switch-time (or (plist-get pdata :switch-time) 0))
                       projects--table)))

          ;; Restore files for each project
          (dolist (proj-entry project-list)
            (let ((name (car proj-entry))
                  (pdata (cdr proj-entry)))
              (setq projects--current name)
              (dolist (file (plist-get pdata :files))
                (when (file-exists-p file)
                  (let ((buf (find-file-noselect file t)))
                    (with-current-buffer buf
                      (setq-local projects--buffer-project name))
                    (let* ((entry (gethash name projects--table))
                           (bufs (plist-get entry :buffers)))
                      (plist-put entry :buffers (cons buf bufs))
                      (puthash name entry projects--table)))
                  (cl-incf current-count)
                  ;; Update progress display
                  (let* ((pct (if (> total 0)
                                  (round (* 100.0 (/ (float current-count) total)))
                                100))
                         (loading-buf (get-buffer "*session-loading*")))
                    (when (and loading-buf (buffer-live-p loading-buf))
                      (with-current-buffer loading-buf
                        (let ((inhibit-read-only t))
                          (when (and (boundp 'progress-start-marker)
                                     (boundp 'progress-end-marker))
                            (delete-region progress-start-marker progress-end-marker)
                            (goto-char progress-start-marker)
                            (insert (propertize (format "%d%% (%d/%d)" pct current-count total)
                                                'face `(:height 1.2 :foreground ,green-color)))
                            (set-marker progress-end-marker (point))))))
                    (redisplay t))))))

          ;; Restore active project
          (when (and saved-current (gethash saved-current projects--table))
            (setq projects--current saved-current))

          ;; Clean up and show dashboard (deferred so progress renders)
          (run-with-timer 0.2 nil
                          (lambda ()
                            (when (get-buffer "*session-loading*")
                              (kill-buffer "*session-loading*"))
                            (when (fboundp 'projects--tab-bar-refresh)
                              (projects--tab-bar-refresh))
                            (when (fboundp 'projects--ensure-visible-buffer)
                              (projects--ensure-visible-buffer))
                            (when (fboundp '+doom-dashboard-reload)
                              (+doom-dashboard-reload t))))))))))
```

**Step 3: Add auto-save timer and hooks**

**Replace** the entire `projects--setup-hooks` function defined in Task 3 with this expanded version
(do not append — replace the whole defun so hooks are only installed once when `(projects--setup-hooks)` is called):

```elisp
(defun projects--setup-hooks ()
  "Install all hooks for buffer tracking, auto-save, and frame management."
  (add-hook 'find-file-hook #'projects--find-file-hook)
  (add-hook 'kill-buffer-hook #'projects--cleanup-dead-buffers)
  ;; Auto-save every 5 minutes
  (run-with-timer 300 300
    (lambda ()
      (when (hash-table-p projects--table)
        (projects-save))))
  ;; Save on Emacs exit
  (add-hook 'kill-emacs-hook #'projects-save)
  ;; Save on last frame close (daemon mode)
  (add-hook 'delete-frame-functions
    (lambda (_frame)
      (when (and (daemonp)
                 (<= (length (filtered-frame-list
                               (lambda (f) (not (frame-parameter f 'parent-frame)))))
                     2))
        (projects-save)))))
```

**Step 4: Update `my/restore-session-on-first-frame` in +functions.el**

Find `my/restore-session-on-first-frame` at line ~970 in `+functions.el` and replace its body call from `my/quickload-session` to `projects-restore`:

```elisp
;; CHANGE: replace (my/quickload-session) with:
(projects-restore)
```

**Step 5: Verify**

- Create a project, open some files, call `(projects-save)`
- Check `~/.config/emacs/.local/etc/projects/session.el` exists and contains project data
- Restart Emacs → `projects-restore` is called, progress buffer appears, files reopen

**Step 6: Commit**
```
feat(emacs/projects): add save/restore with animated progress display
```

---

### Task 7: Keybindings

**Depends on:** Tasks 2, 3

**Files:**
- Modify: `.doom.d/config.el`

**Step 1: Replace workspace keybindings**

In `.doom.d/config.el`, find the existing keybinding block:
```elisp
;; workspace switching (MRU sorted)
:leader
(:prefix ("w" . "workspaces")
 :desc "Switch workspace (MRU)" "w" #'my/workspace-switch-to-mru)
```

Replace it with a comprehensive projects keymap:

```elisp
;; Projects system keybindings (replaces persp-mode workspace bindings)
:leader
(:prefix ("w" . "projects")
 :desc "Switch project (MRU)"      "w" #'projects-switch
 :desc "New project"                "n" #'projects-create
 :desc "Rename project"             "r" #'projects-rename
 :desc "Delete project"             "k" #'projects-delete
 :desc "Move buffer to project"     "m" #'projects-move-buffer
 :desc "Switch buffer in project"   "b" #'projects-switch-buffer
 :desc "Save projects state"        "s" #'projects-save
 :desc "Restore projects session"   "R" #'projects-restore)
```

**Step 2: Add C-c w prefix map**

After the `:leader` block, add a standard `C-c w` keymap:

```elisp
;; C-c w prefix for projects (non-evil users)
(define-prefix-command 'projects-map)
(global-set-key (kbd "C-c w") 'projects-map)
(define-key projects-map (kbd "w") #'projects-switch)
(define-key projects-map (kbd "n") #'projects-create)
(define-key projects-map (kbd "r") #'projects-rename)
(define-key projects-map (kbd "k") #'projects-delete)
(define-key projects-map (kbd "m") #'projects-move-buffer)
(define-key projects-map (kbd "b") #'projects-switch-buffer)
(define-key projects-map (kbd "s") #'projects-save)
(define-key projects-map (kbd "R") #'projects-restore)

;; Override C-x b to use project-aware buffer switching
(global-set-key (kbd "C-x b") #'projects-switch-buffer)
```

**Step 3: Remove old quickload binding**

In `.doom.d/config.el`, find the standalone `"C-c w Q" #'my/quickload-session` line (around line 290, inside the global `map!` block). Remove it — the restore function is now bound to `C-c w R` via the projects prefix map defined in Step 2 of this task. Also remove the corresponding line in `config.org` at line ~364.

**Step 4: Verify**

- `C-c w w` invokes `projects-switch`
- `C-c w n` invokes `projects-create`
- `C-x b` invokes `projects-switch-buffer` (filters to current project buffers)

**Step 5: Commit**
```
feat(emacs/projects): add keybindings for projects system
```

---

### Task 8: ibuffer integration

**Depends on:** Task 3

**Files:**
- Modify: `.doom.d/projects.el`
- Modify: `.doom.d/config.el`

**Step 1: Add ibuffer group function to projects.el**

Add before `;;; Setup Hooks`:

```elisp
;;; ---------------------------------------------------------------------------
;;; ibuffer Integration
;;; ---------------------------------------------------------------------------

(defun projects-ibuffer-groups ()
  "Return ibuffer filter groups for all current projects.
Each project gets its own group. Special buffers go in 'Global'.

ibuffer predicate filter format: (GROUP-NAME (predicate . FUNC))
where FUNC is a function receiving a buffer argument and returning non-nil if the
buffer belongs to the group. Use a closure to capture the project name per group."
  (append
   (mapcar (lambda (name)
             (let ((captured name))
               (list captured
                     `(predicate . ,(lambda (buf)
                                      (equal (buffer-local-value 'projects--buffer-project buf)
                                             captured))))))
           (projects-names-mru))
   `(("Global"
      (predicate . ,(lambda (buf)
                      (null (buffer-local-value 'projects--buffer-project buf))))))))

(defun projects--ibuffer-setup ()
  "Set up ibuffer to use project groups."
  (setq ibuffer-filter-groups (projects-ibuffer-groups))
  (ibuffer-update nil t))
```

**Step 2: Install ibuffer hook in config.el**

In `.doom.d/config.el`, find the `(after! ibuffer ...)` block (around line 587) and add inside it:

```elisp
;; Group buffers by project
(add-hook 'ibuffer-hook #'projects--ibuffer-setup)
```

**Step 3: Verify**

- Open ibuffer (`C-x C-b` or `:ilist`)
- Buffers should be grouped by project name
- Special buffers appear under "Global"

**Step 4: Commit**
```
feat(emacs/projects): add ibuffer grouping by project
```

---

### Task 9: Doom dashboard integration

**Depends on:** Task 2

**Files:**
- Modify: `.doom.d/+functions.el`

**Step 1: Update `my/doom-dashboard-widget-projects`**

Find `my/doom-dashboard-widget-projects` at line ~637 in `+functions.el` and replace the entire function with a version using the new `projects` API:

```elisp
;; Dashboard project widget — uses projects.el instead of persp-mode
(defun my/doom-dashboard-widget-projects ()
  "Custom widget showing all projects as clickable buttons, sorted by last usage."
  (when (fboundp 'projects-names-mru)
    (let* ((names (projects-names-mru))
           (content-width 75))
      (when names
        (insert "\n")
        (insert (+doom-dashboard--center
                 +doom-dashboard--width
                 (propertize "Projects:" 'face 'doom-dashboard-menu-title)))
        (insert "\n\n")
        (dolist (name names)
          (let* ((dir (or (projects-dir name) ""))
                 (dir-display (abbreviate-file-name dir))
                 (spacing (max 1 (- content-width (length name) (length dir-display))))
                 (captured-name name))
            (insert
             (+doom-dashboard--center
              +doom-dashboard--width
              (with-temp-buffer
                (insert-text-button
                 name
                 'action (lambda (_button)
                           (let ((win (get-buffer-window +doom-dashboard-name)))
                             (when win
                               (with-selected-window win
                                 (quit-window t))))
                           (projects-switch captured-name t))
                 'follow-link t
                 'face 'doom-dashboard-menu-desc
                 'mouse-face 'doom-dashboard-menu-title
                 'help-echo (format "Switch to project: %s → %s" name dir-display))
                (insert (make-string spacing ?\s))
                (insert (propertize dir-display 'face 'font-lock-comment-face))
                (buffer-string))))
            (insert "\n")))))))
```

**Step 2: Remove or update `my/update-workspace-switch-time`**

This function in `+functions.el` at line ~707 uses `persp-mode`. It's now handled by `projects-switch`. Either remove it or guard it:

Replace the hook installation in `config.el`:
```elisp
;; REMOVE: (add-hook 'persp-activated-functions #'my/update-workspace-switch-time)
```

**Step 3: Verify**

- Open Doom dashboard (`C-c h h` or restart Emacs)
- Dashboard shows "Projects:" section with project names and directories
- Clicking a project name switches to it

**Step 4: Commit**
```
feat(emacs/projects): update dashboard widget for projects system
```

---

### Task 10: Remove all persp-mode code

**Depends on:** Tasks 1-9 (all tasks complete)

**Files:**
- Modify: `.doom.d/config.el`
- Modify: `.doom.d/+functions.el`

**Step 1: Remove from config.el**

Remove or replace the following blocks entirely:

1. `(add-hook 'persp-activated-functions #'my/update-workspace-switch-time)` (line ~82)
2. The `(after! persp-mode ...)` block for auto-save (lines ~96-134):
   - `persp-emacsclient-init-frame-behaviour-override`
   - `persp-auto-save-opt` / `persp-auto-save-num-of-backups`
   - The 5-minute timer using `persp-save-state-to-file`
   - The `delete-frame-functions` hook using `persp-save-state-to-file`
3. The `(after! persp-mode ...)` block for tab-bar (lines ~131-185) — **already replaced in Task 5**

**Step 2: Remove from +functions.el**

Remove these functions from `+functions.el` (they are now replaced by `projects.el`):
- `my/update-workspace-switch-time` (line ~707) — replaced by `projects-switch`
- `my/workspace-switch-to-mru` (line ~713) — replaced by `projects-switch`
- `my/workspace-names-mru-sorted` (line ~741) — replaced by `projects-names-mru`
- `my/tab-bar-workspaces` (line ~754) — replaced by `projects--tab-bar-format`
- `my/workspace-bar-refresh` (line ~772) — replaced by `projects--tab-bar-refresh`
- `my/switch-to-workspace-for-directory` (line ~931) — no longer needed (buffer always goes to current project)

**Keep** in `+functions.el`:
- `my/quickload-session` (line ~296) — keep for backward compat or remove if `projects-restore` fully replaces it
- `my/doom-dashboard-widget-projects` (line ~637) — already updated in Task 9
- `my/workspace-tab-active` and `my/workspace-tab-inactive` face definitions (line ~1051) — KEEP (reused by projects.el)
- `my/restore-session-on-first-frame` (line ~970) — KEEP but update to call `projects-restore` (done in Task 6)

**Step 3: Update config.org to match config.el**

Since `config.org` is the source for `config.el`, update `config.org` to match the changes already made to `config.el` in Tasks 1-9. Edit the org source blocks directly — do NOT run `org-babel-tangle` (it would overwrite `config.el` with the old persp-mode code if `config.org` is not fully updated first).

Changes to make in `config.org`:
- `** Workspace Bar` section: replace the `(after! persp-mode ...)` tab-bar block with the `(after! projects ...)` block from Task 5
- Remove or comment out the `(add-hook 'persp-activated-functions ...)` line
- Remove the `(after! persp-mode ...)` auto-save block
- Update the keybindings `** Keybindings` section to replace `workspaces` with `projects` bindings from Task 7

**Step 4: Verify**

```
doom sync
```
Start Emacs. Verify:
- No `persp-mode` related errors or warnings
- `(require 'persp-mode)` → `File not found` (disabled)
- Tab-bar shows projects
- `C-c w w` works
- ibuffer groups by project

**Step 5: Commit**
```
refactor(emacs/projects): remove all persp-mode code, projects system is primary
```

---

## Task Dependency Graph

```
Task 1 (scaffold) ──> Task 2 (data model) ──> Task 3 (buffer tracking) ──> Task 7 (keybindings)
                                          │                              └──> Task 8 (ibuffer)
                                          ├──> Task 4 (info buffer)
                                          ├──> Task 5 (tab-bar)
                                          └──> Task 9 (dashboard)

Task 6 (persistence) depends on: Task 2, 3, 4, 5

Task 10 (cleanup) depends on: ALL tasks complete
```

## Open Questions

- None — all requirements clarified during interview.

## Notes for Executor

- After each task, run `doom sync` if `init.el` or `packages.el` was changed, otherwise `M-x doom/reload` is sufficient.
- `config.org` is the source of truth for `config.el` (org-babel-tangle). For this plan, edit `config.el` directly and update `config.org` in Task 10. This avoids tangling breaking your in-progress config.
- `projects.el` is a standalone file — all features are added to it incrementally across tasks.
- The progress display in Task 6 reuses `doom-color` — this is available after theme loads. If called during early init, wrap with `(if (fboundp 'doom-color) ...)`.