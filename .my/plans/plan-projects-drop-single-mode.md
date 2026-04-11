# Drop Single-Project Mode Implementation Plan

**Goal:** Eliminate the dual single/multi project mode architecture, making multi-project mode (with window-level project ownership) the only mode. Add "1x1" layout as the default single-window experience.
**Type:** Feature
**JIRA:** none
**Tech Stack:** Emacs Lisp (Doom Emacs config)
**Code Standards:** n/a
**Status:** DONE
**no-worktree:** [ ]
**no-pr:** [ ]
**draft-pr:** [ ]
**merge:** [x]
**merge-admin:** [ ]
**non-interactive:** [x]
**execution:** remote
**add-marketplaces:** anthropics/claude-plugins-official, parloa/claudes-kitchen, JuliusBrussee/caveman, andreas-pohl-parloa/my-coding, andreas-pohl-parloa/plan-executor-plugin
**add-plugins:** backend-services@claudes-kitchen, career-development@claudes-kitchen, caveman@caveman, gateway@claudes-kitchen, go-services@claudes-kitchen, my@my-coding, operational-excellence@claudes-kitchen, parloa-toolkit-services@claudes-kitchen, plan-executor@plan-executor, playwright@claude-plugins-official, python-services@claudes-kitchen, rust-services@claudes-kitchen, security@claudes-kitchen, skills-development@claudes-kitchen, superpowers@claude-plugins-official, threat-modeling@claudes-kitchen, typescript-services@claudes-kitchen, workflows@claudes-kitchen

---

**Architecture:** Every window always has a `projects-project` window parameter. "Single project" is just a 1x1 layout. `projects-switch` becomes `projects-switch-window-project` — it changes the current window's project assignment and replaces its buffer. No more `window-configuration` save/restore. No more `projects-view-mode` branching.

### File map

- **Modify:** `.doom.d/projects.el` — all changes are here
- **Modify:** `.doom.d/config.el` — keybinding updates (lines ~344-360, ~166-172)

---

### Task 1: Add "1x1" layout and update layout constants

**Files:**
- Modify: `.doom.d/projects.el:385-397` (layout constants)

- [ ] **Step 1: Update `projects--multi-layouts` to include "1x1"**

```elisp
(defconst projects--multi-layouts '("1x1" "2x1" "2x2" "3x2")
  "Available project layout names, in display order.")
```

- [ ] **Step 2: Update `projects--layout-window-count` to handle "1x1"**

```elisp
(defun projects--layout-window-count (layout)
  "Return the number of windows for LAYOUT string (e.g. \"2x1\" → 2)."
  (pcase layout
    ("1x1" 1)
    ("2x1" 2)
    ("2x2" 4)
    ("3x2" 6)
    (_ 1)))
```

- [ ] **Step 3: Update `projects--split-for-layout` to handle "1x1"**

The "1x1" layout just calls `delete-other-windows` — no splitting needed. The function already starts with `delete-other-windows`, so "1x1" is a no-op after that:

```elisp
(defun projects--split-for-layout (layout)
  (delete-other-windows)
  (pcase layout
    ("1x1" nil)  ; single window, nothing to split
    ("2x1" (split-window-right))
    ("2x2" (split-window-right)
            (other-window 1)
            (split-window-below)
            (other-window -1)
            (split-window-below))
    ("3x2" (let* ((c1    (selected-window))
                   (col-w (/ (window-total-width c1) 3)))
             (select-window c1)
             (split-window-right col-w)
             (let ((c2 (next-window c1)))
               (select-window c2)
               (split-window-right col-w)
               (let ((c3 (next-window c2)))
                 (select-window c1) (split-window-below)
                 (select-window c2) (split-window-below)
                 (select-window c3) (split-window-below)
                 (select-window c1)))))))
```

- [ ] **Step 4: Commit**

```bash
git add .doom.d/projects.el
git commit -m "feat(projects): add 1x1 layout to multi-layouts"
```

---

### Task 2: Remove `projects-view-mode` and always-on multi-project mode

**Files:**
- Modify: `.doom.d/projects.el:66-109` (accessors, enter-single-project-view)

- [ ] **Step 1: Remove `projects-view-mode` accessor, make `projects-multi-project-view-p` always return t**

Delete `projects-view-mode` and replace `projects-multi-project-view-p` with a constant:

```elisp
(defun projects-multi-project-view-p (&optional _frame)
  "Always t — all windows use per-window project ownership."
  t)
```

- [ ] **Step 2: Remove `projects--set-view-mode`**

Delete the function entirely (`.doom.d/projects.el:81-82`):

```elisp
;; DELETE:
;; (defun projects--set-view-mode (mode &optional frame)
;;   (set-frame-parameter (or frame (selected-frame)) 'projects-view-mode mode))
```

- [ ] **Step 3: Remove `projects-enter-single-project-view`**

Delete the entire function (`.doom.d/projects.el:94-109`):

```elisp
;; DELETE the entire projects-enter-single-project-view function
```

- [ ] **Step 4: Remove `projects--clear-window-project-params`**

No longer needed — window project params are always valid. Delete (`.doom.d/projects.el:87-92`):

```elisp
;; DELETE the entire projects--clear-window-project-params function
```

- [ ] **Step 5: Commit**

```bash
git add .doom.d/projects.el
git commit -m "refactor(projects): remove single-project mode, always use per-window ownership"
```

---

### Task 3: Rewrite `projects-switch` — merge with `projects-switch-window-project`

This is the core change. `projects-switch` becomes window-scoped: it changes the current window's project assignment and shows a project buffer. No window-configuration save/restore.

**Files:**
- Modify: `.doom.d/projects.el:306-383` (projects-switch)

- [ ] **Step 1: Rewrite `projects-switch`**

Replace the entire function body. The new logic:
1. Update frame-level current project tracking
2. Update the current window's `projects-project` parameter
3. Show a project buffer in the current window
4. Set `default-directory`
5. Refresh UI (tab-bar, header-line)

```elisp
(defun projects-switch (name &optional norecord)
  "Switch the current window to project NAME.
Updates the window's project assignment, shows a project buffer,
and updates frame-level tracking. Tab-bar and header-line are refreshed."
  (interactive
   (progn
     (when (projects-hidden-p (projects-current))
       (user-error "Cannot switch projects from a temporary project"))
     (let ((candidates (cl-remove (projects-current-window-project)
                                  (projects-names-visible) :test #'equal)))
       (list (completing-read "Switch to project: "
                              (lambda (str pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity))
                                  (complete-with-action action candidates str pred)))
                              nil t)))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (when (called-interactively-p 'any)
    (when (projects-hidden-p (projects-current))
      (user-error "Cannot switch projects from a temporary project"))
    (when (projects-hidden-p name)
      (user-error "Cannot switch to a hidden project")))
  (message "[projects] switch: %s -> %s%s (caller: %s)"
           (projects-current-window-project) name
           (if norecord " (norecord)" "")
           (or (ignore-errors (cadr (backtrace-frame 2))) "?"))
  ;; Update frame-level current project
  (projects--set-current name)
  ;; Update switch time
  (unless norecord
    (let ((proj (gethash name projects--table)))
      (plist-put proj :switch-time (float-time))
      (puthash name proj projects--table)))
  ;; Set global default-directory
  (unless (projects-hidden-p name)
    (when-let ((dir (projects-dir name)))
      (setq-default default-directory dir)))
  ;; Update this window's project assignment and show a project buffer
  (projects--set-window-project (selected-window) name)
  (switch-to-buffer (projects--window-buffer-for-project name))
  ;; Refresh UI
  (projects--refresh-window-project-headers)
  (projects--update-frame-tab-bar)
  (projects--tab-bar-refresh)
  (run-hooks 'projects-switch-hook))
```

- [ ] **Step 2: Remove `projects-switch-window-project` (now redundant)**

`projects-switch` does what `projects-switch-window-project` did. Delete the standalone function (`.doom.d/projects.el:489-496`):

```elisp
;; DELETE the entire projects-switch-window-project function
```

- [ ] **Step 3: Simplify `projects-switch-dispatch`**

It's now just `projects-switch`:

```elisp
(defun projects-switch-dispatch ()
  (interactive)
  (call-interactively #'projects-switch))
```

Or replace all references to `projects-switch-dispatch` with `projects-switch` directly and delete the function. The simpler approach: just make it an alias for now to avoid breaking keybindings.

- [ ] **Step 4: Remove `projects--ensure-visible-buffer`**

No longer needed — `projects-switch` only changes the current window, and `projects--window-buffer-for-project` already finds the right buffer. Delete the function (`.doom.d/projects.el:847-884`).

- [ ] **Step 5: Remove `:window-config` from save/restore**

In `projects-save` (around line 1026-1052): the `:window-config` plist key is never set anymore (no more `current-window-configuration` calls), so this is already inert. But clean up any references.

Search for `window-config` in projects.el and remove all references. The save function doesn't persist `window-config` (it's not serializable), but the switch function used to set it on the entry plist — that code is gone now.

- [ ] **Step 6: Commit**

```bash
git add .doom.d/projects.el
git commit -m "refactor(projects): rewrite projects-switch as window-scoped operation"
```

---

### Task 4: Remove all dead `projects-multi-project-view-p` branches

Now that `projects-multi-project-view-p` always returns t, simplify all callers.

**Files:**
- Modify: `.doom.d/projects.el` (multiple locations)

- [ ] **Step 1: Simplify `projects-current-window-project`**

The fallback `(projects-current (window-frame win))` is for when there's no window parameter. Keep it as-is — it's still useful as a fallback.

No change needed.

- [ ] **Step 2: Simplify `projects--find-file-hook`**

The `multi` variable is now always t. Remove the `(multi ...)` binding and the single-project branch. The function becomes:

```elisp
(defun projects--find-file-hook ()
  "Register newly opened files with the current project."
  (let ((buf (current-buffer))
        (win-proj (window-parameter (selected-window) 'projects-project)))
    (message "[projects] find-file-hook: buf=%s selected-win=%s win-proj=%s frame-proj=%s"
             (buffer-name buf) (selected-window) win-proj (projects-current))
    (cond
     ;; Fresh client frame opened with a file via emacsclient (no project yet).
     ((and (frame-parameter nil 'client)
           (null (frame-parameter nil 'projects-current)))
      (message "[projects] find-file-hook: routing to tmp (fresh client frame)")
      (projects--ensure-tmp-project)
      (projects-register-buffer buf "tmp")
      (projects--set-current "tmp" t)
      (projects--update-frame-tab-bar))
     ;; No active project at all
     ((null (projects-current))
      (message "[projects] find-file-hook: routing to tmp (no current project)")
      (projects--ensure-tmp-project)
      (projects-register-buffer buf "tmp")
      (projects-switch "tmp"))
     ;; Known window project: register immediately
     (win-proj
      (message "[projects] find-file-hook: registering buf=%s to win-proj=%s"
               (buffer-name buf) win-proj)
      (projects-register-buffer buf win-proj))
     ;; No window project yet — defer to window-buffer-change-hook
     (t
      (message "[projects] find-file-hook: deferring registration (no win-proj)")))))
```

- [ ] **Step 3: Simplify `projects--window-buffer-change-hook`**

Remove the `(when (projects-multi-project-view-p frame)` guard — it's always true:

```elisp
(defun projects--window-buffer-change-hook (frame)
  "Re-register buffers appearing in windows with the correct window project,
and refresh the header-line-format."
  (let ((refreshed nil))
    (dolist (win (window-list frame 0))
      (let* ((buf      (window-buffer win))
             (win-proj (window-parameter win 'projects-project))
             (buf-proj (buffer-local-value 'projects--buffer-project buf)))
        (when win-proj
          (when (and (not (projects-special-buffer-p buf))
                     (not (string-match-p "^\\*project: " (buffer-name buf)))
                     (null buf-proj))
            (message "[projects] window-buffer-change: win=%s buf=%s old-proj=nil new-proj=%s"
                     win (buffer-name buf) win-proj)
            (projects-register-buffer buf win-proj))
          (with-current-buffer buf
            (unless (equal header-line-format '(:eval (projects--window-header-line)))
              (setq header-line-format '(:eval (projects--window-header-line)))
              (setq refreshed t))))))
    (when refreshed
      (force-mode-line-update t))))
```

- [ ] **Step 4: Simplify `projects--cleanup-dead-buffers`**

Remove the `(when (projects-multi-project-view-p)` guard around the pre-set logic — pre-set always runs:

```elisp
(defun projects--cleanup-dead-buffers ()
  "Remove the dying buffer from its project and pre-set replacement in windows."
  (let ((proj projects--buffer-project)
        (buf (current-buffer)))
    (message "[projects] cleanup-dead: buf=%s proj=%s in-table=%s"
             (buffer-name buf) proj
             (and proj (memq buf (plist-get (gethash proj projects--table) :buffers)) t))
    (when proj
      (let* ((entry (gethash proj projects--table))
             (bufs (plist-get entry :buffers)))
        (when (memq buf bufs)
          (message "[projects] buffer-killed: %s from project %s" (buffer-name buf) proj)
          (plist-put entry :buffers (delq buf bufs))
          (puthash proj entry projects--table)
          ;; Pre-set windows showing the dying buffer to a project-local replacement
          (let* ((remaining (plist-get (gethash proj projects--table) :buffers))
                 (replacement (cl-find-if
                               (lambda (b)
                                 (and (buffer-live-p b)
                                      (not (eq b buf))
                                      (not (get-buffer-window b))
                                      (equal (buffer-local-value 'projects--buffer-project b) proj)))
                               remaining)))
            (dolist (win (get-buffer-window-list buf nil t))
              (let ((rep (or replacement (projects--create-info-buffer proj))))
                (message "[projects] buffer-killed: pre-setting win=%s to %s" win (buffer-name rep))
                (set-window-buffer win rep))))
          ;; Deferred safety net
          (run-with-timer 0 nil #'projects--fix-windows-after-kill))))))
```

- [ ] **Step 5: Simplify `projects--window-target-project`**

Always use window project:

```elisp
(defun projects--window-target-project (window)
  (projects-current-window-project window))
```

- [ ] **Step 6: Simplify `projects--set-window-project-dir`**

Remove the `(when (projects-multi-project-view-p)` guard:

```elisp
(defun projects--set-window-project-dir (&rest _)
  "Set `default-directory' to the current window's project root.
Used as :before advice on vterm/eat so the terminal opens in the right dir."
  (when-let* ((proj (projects-current-window-project))
              (dir  (projects-dir proj)))
    (message "[projects] set-window-project-dir: %s -> %s" proj dir)
    (setq default-directory dir)))
```

- [ ] **Step 7: Simplify `projects--refresh-window-project-headers`**

Remove the conditional — header-line is always set:

```elisp
(defun projects--refresh-window-project-headers ()
  (dolist (win (window-list nil 0))
    (with-current-buffer (window-buffer win)
      (setq header-line-format '(:eval (projects--window-header-line)))))
  (force-mode-line-update t))
```

- [ ] **Step 8: Simplify `projects--maybe-close-info-window`**

Remove the multi-project skip guard. In 1x1 layout this auto-closes the info buffer when another window appears (e.g., claude split). In multi-window layouts, we should still skip — check window count vs layout window count instead:

```elisp
(defun projects--maybe-close-info-window ()
  "Close project info buffer window when more windows exist than the layout needs."
  (let* ((layout (or (frame-parameter nil 'projects-multi-layout) "1x1"))
         (expected (projects--layout-window-count layout)))
    (when (> (projects--ordinary-window-count) expected)
      (let ((info-win
             (cl-find-if
              (lambda (w)
                (string-match-p "^\\*project: "
                                (buffer-name (window-buffer w))))
              (window-list nil 0))))
        (when info-win
          (run-with-idle-timer 0 nil
                               (lambda (win)
                                 (when (and (window-live-p win)
                                            (> (projects--ordinary-window-count) expected))
                                   (delete-window win)))
                               info-win))))))
```

Note: also handle the fresh-client-frame case if it's still needed.

- [ ] **Step 9: Simplify `projects--update-frame-tab-bar`**

Since every window has a header-line now, and the tab-bar shows all projects for switching — keep both. Remove the `multi` check that hides it:

```elisp
(defun projects--update-frame-tab-bar (&optional frame)
  "Show or hide the tab-bar for FRAME based on whether its project is hidden."
  (let* ((f    (or frame (selected-frame)))
         (proj (projects-current f))
         (hide (and proj (projects-hidden-p proj))))
    (set-frame-parameter f 'tab-bar-lines (if hide 0 1))))
```

- [ ] **Step 10: Simplify `projects-enter-multi-project-view` → rename to `projects-set-layout`**

```elisp
(defun projects-set-layout (layout)
  "Change the window layout to LAYOUT, preserving project assignments where possible."
  (interactive (list (projects--read-multi-layout)))
  (unless (projects--valid-multi-layout-p layout)
    (user-error "Unsupported layout: %s" layout))
  (projects--set-multi-layout layout)
  (projects--apply-multi-project-layout layout)
  (projects--refresh-window-project-headers)
  (projects--update-frame-tab-bar)
  (projects--tab-bar-refresh))
```

- [ ] **Step 11: Remove `projects-change-multi-project-layout` (redundant with `projects-set-layout`)**

Delete it — `projects-set-layout` replaces both `projects-enter-multi-project-view` and `projects-change-multi-project-layout`.

- [ ] **Step 12: Commit**

```bash
git add .doom.d/projects.el
git commit -m "refactor(projects): remove all multi-project-view-p branches"
```

---

### Task 5: Update save/restore for always-multi mode

**Files:**
- Modify: `.doom.d/projects.el:1020-1212` (save/restore)

- [ ] **Step 1: Update `projects-save` — remove `:view-mode`**

In `projects-save`, the persisted data includes `:view-mode`. Remove it — it's always multi. Keep `:multi-layout` and `:window-projects`:

In the `pp` form, change:
```elisp
;; Remove :view-mode
;; Keep :multi-layout and :window-projects as they are
(pp (list :version 2
          :current projects--current
          :projects data
          :layout (or (frame-parameter nil 'projects-multi-layout) "1x1")
          :window-projects (mapcar (lambda (win)
                                     (window-parameter win 'projects-project))
                                   (window-list nil 0)))
    (current-buffer))
```

Note: bump version to 2 so old sessions trigger a clean restore path.

- [ ] **Step 2: Update `projects-restore` — handle version 2 and legacy**

In the restore function, after reading project data:

```elisp
;; Restore layout
(let* ((saved-layout (or (plist-get data :layout)
                         (plist-get data :multi-layout)
                         "1x1"))
       (saved-window-projects (plist-get data :window-projects)))
  (projects--set-multi-layout saved-layout)
  (when saved-window-projects
    (projects--apply-multi-project-layout saved-layout saved-window-projects))
  (projects--refresh-window-project-headers)
  (projects--update-frame-tab-bar))
```

Remove the `(if (eq saved-view-mode 'multi-project) ...)` branching and the `(projects--set-view-mode ...)` calls.

- [ ] **Step 3: Commit**

```bash
git add .doom.d/projects.el
git commit -m "refactor(projects): update save/restore for unified layout model"
```

---

### Task 6: Update `projects-create` and `projects-delete` for window-scoped switching

**Files:**
- Modify: `.doom.d/projects.el:171-310`

- [ ] **Step 1: Check `projects-create`**

Read the function and verify it calls `projects-switch` at the end. Since `projects-switch` is now window-scoped, creating a project and switching to it will correctly set the current window's project. No change likely needed — just verify.

- [ ] **Step 2: Check `projects-delete`**

Read the function and verify it handles multi-window correctly. When deleting a project that's assigned to a window, those windows need reassignment. Verify this works.

- [ ] **Step 3: Commit if changes were needed**

```bash
git add .doom.d/projects.el
git commit -m "fix(projects): update create/delete for window-scoped model"
```

---

### Task 7: Ensure initial startup assigns window project

**Files:**
- Modify: `.doom.d/projects.el` (restore path, config.el startup hooks)

- [ ] **Step 1: After restore, ensure every window has a `projects-project` parameter**

The `projects--apply-multi-project-layout` already does this. But after a fresh start with no saved session, the first project created needs to set the window parameter. Check that `projects-switch` (called from create) sets the window param.

Already handled in the new `projects-switch` — it calls `(projects--set-window-project (selected-window) name)`.

- [ ] **Step 2: Set default layout on startup when no session exists**

In the startup path (config.el `doom-after-init-hook`), when no session is restored, ensure the frame is in "1x1" layout:

```elisp
;; In the doom-after-init-hook lambda, after projects-restore:
(unless (frame-parameter nil 'projects-multi-layout)
  (projects--set-multi-layout "1x1"))
```

- [ ] **Step 3: Commit**

```bash
git add .doom.d/projects.el .doom.d/config.el
git commit -m "feat(projects): ensure 1x1 default layout on startup"
```

---

### Task 8: Update keybindings in config.el

**Files:**
- Modify: `.doom.d/config.el:343-361`

- [ ] **Step 1: Update the projects keybinding prefix**

Replace the current bindings with the new layout-based keys:

```elisp
(:prefix ("w" . "projects")
 :desc "Switch project"             "w" #'projects-switch
 :desc "New project"                "n" #'projects-create
 :desc "Rename project"             "r" #'projects-rename
 :desc "Delete project"             "k" #'projects-delete
 :desc "Move buffer to project"     "m" #'projects-move-buffer
 :desc "Switch buffer in project"   "b" #'projects-switch-buffer
 :desc "Save projects state"        "s" #'projects-save
 :desc "Restore projects session"   "R" #'projects-restore
 :desc "Restore from backup…"       "l" (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'projects-restore)))
 :desc "Project info buffer"        "i" #'projects-show-info
 :desc "Clone from git"             "g" #'projects-clone-from-git
 :desc "Layout: 1x1"                "1" (lambda () (interactive) (projects-set-layout "1x1"))
 :desc "Layout: 2x1"                "2" (lambda () (interactive) (projects-set-layout "2x1"))
 :desc "Layout: 2x2"                "3" (lambda () (interactive) (projects-set-layout "2x2"))
 :desc "Layout: 3x2"                "4" (lambda () (interactive) (projects-set-layout "3x2")))
```

Remove the old `"a"` (duplicate save), `"L"` (change layout), `"2"` (enter multi-view), `"1"` (enter single-view) bindings.

- [ ] **Step 2: Remove old references**

Remove `projects-enter-single-project-view` and `projects-enter-multi-project-view` from any remaining references in config.el.

- [ ] **Step 3: Commit**

```bash
git add .doom.d/config.el
git commit -m "feat(projects): update keybindings for unified layout model"
```

---

### Task 9: Update `projects--auto-switch-on-display` and claude-code advice

**Files:**
- Modify: `.doom.d/projects.el:1218-1287`

- [ ] **Step 1: Simplify `projects--auto-switch-on-display`**

This function auto-switches the frame-level project when a buffer from a different project appears. In the new model, we should NOT auto-switch the frame project — each window owns its own project. However, we should update the frame-level `projects-current` to track which window has focus (for `default-directory` and tab-bar highlighting).

Keep the function but scope it to frame-level tracking only — don't change window assignments:

```elisp
(defun projects--auto-switch-on-display (frame)
  "Track the focused window's project as the frame-level current project.
Updates default-directory and tab-bar highlighting."
  (let* ((buf      (window-buffer (frame-selected-window frame)))
         (buf-proj (buffer-local-value 'projects--buffer-project buf))
         (frame-proj (projects-current frame)))
    (when (and buf-proj
               (not (equal buf-proj frame-proj))
               (gethash buf-proj projects--table)
               (not (projects-hidden-p buf-proj))
               (not (projects-hidden-p frame-proj)))
      (message "[projects] auto-switch frame: %s -> %s (buffer: %s)"
               frame-proj buf-proj (buffer-name buf))
      (set-frame-parameter frame 'projects-current buf-proj)
      (unless (frame-parameter frame 'client)
        (setq projects--current buf-proj))
      (unless (projects-hidden-p buf-proj)
        (when-let ((dir (projects-dir buf-proj)))
          (setq-default default-directory dir)))
      (projects--update-frame-tab-bar frame)
      (run-with-timer 0 nil #'projects--tab-bar-refresh))))
```

- [ ] **Step 2: Simplify claude-code--directory advice**

Remove the `(projects-multi-project-view-p)` check — always use window project:

```elisp
(advice-add 'claude-code--directory :around
            (lambda (orig)
              (if-let ((proj (projects-current-window-project))
                       (dir (projects-dir proj)))
                  (progn
                    (message "[projects] claude-code--directory override: %s -> %s" proj dir)
                    dir)
                (funcall orig))))
```

- [ ] **Step 3: Commit**

```bash
git add .doom.d/projects.el
git commit -m "refactor(projects): simplify auto-switch and claude-code advice"
```

---

### Task 10: Remove diagnostic logging

**Files:**
- Modify: `.doom.d/projects.el` (throughout)

- [ ] **Step 1: Remove verbose `message` calls**

Go through the file and remove or reduce the diagnostic `(message "[projects] ...")` calls that were added during debugging. Keep only error-level messages. A few key status messages can stay (e.g., project switch notifications).

Key messages to keep:
- `[projects] switch:` (project switch events)
- `[projects] buffer-killed:` (buffer cleanup)

Messages to remove:
- `[projects] register-buffer:` (too noisy)
- `[projects] find-file-hook:` (too noisy)
- `[projects] cleanup-dead:` (diagnostic)
- `[projects] fix-windows:` (diagnostic)
- `[projects] window-buffer-change:` (diagnostic)
- `[projects] set-window-project-dir:` (diagnostic)
- `[projects] claude-code--directory override:` (diagnostic)
- `[projects] update-tab-bar:` (diagnostic)
- `[projects] auto-switch:` (diagnostic)

- [ ] **Step 2: Commit**

```bash
git add .doom.d/projects.el
git commit -m "chore(projects): remove diagnostic logging"
```

---

### Task 11: Test the full flow

- [ ] **Step 1: Restart Emacs and verify session restore**

Run: Restart Emacs (`emacsclient -t` or daemon restart)
Expected: Session restores with saved layout and window project assignments. Header-lines show project names per window.

- [ ] **Step 2: Test 1x1 layout (default)**

Run: `C-c w 1`
Expected: Single window with header-line showing current project name.

- [ ] **Step 3: Test switching projects in 1x1**

Run: `M-TAB` or `C-c w w`, select a different project
Expected: Current window switches to the new project's buffer. Header-line updates.

- [ ] **Step 4: Test 2x1 layout**

Run: `C-c w 2`
Expected: Two side-by-side windows, each with a different project's header-line.

- [ ] **Step 5: Test opening/closing buffers**

In 2x1 layout:
- Open a file in one window (`C-x C-f`)
- Verify it registers to that window's project
- Kill the buffer (`C-x k`)
- Verify the window shows another buffer from the same project (not from the other window's project)

- [ ] **Step 6: Test vterm/claude in project window**

- Open vterm in a project window (`v` from info screen)
- Verify `default-directory` is the project's dir
- Close vterm
- Verify the window stays on the same project (no cross-contamination)

- [ ] **Step 7: Test layout changes**

- Start in 1x1 with project A
- Switch to 2x1 (`C-c w 2`) — should get A + next MRU project
- Switch to 2x2 (`C-c w 3`) — should get 4 windows with projects
- Switch back to 1x1 (`C-c w 1`) — should collapse to single window with focused project

- [ ] **Step 8: Final commit**

```bash
git add .doom.d/projects.el .doom.d/config.el
git commit -m "feat(projects): unified layout model - drop single-project mode"
```
