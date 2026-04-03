;;; projects.el --- Custom project/workspace system for Doom Emacs -*- lexical-binding: t; -*-
;;
;; Replaces persp-mode with a buffer-strict, directory-bound project system.
;; Each project owns its buffers. Buffers cannot migrate between projects.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------

(defconst projects--save-file
  (expand-file-name "projects/session.el" doom-data-dir)
  "File where project state is persisted.")

(defconst projects--backup-count 4
  "Number of rotating backups to keep alongside `projects--save-file'.")

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
    "^COMMIT_EDITMSG$"
    "^ \\*Minibuf")
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
;;; Accessors
;;; ---------------------------------------------------------------------------

(defun projects-current (&optional frame)
  "Return the active project for FRAME (default: selected frame).
Falls back to the global `projects--current' if no frame-local project is set."
  (or (frame-parameter (or frame (selected-frame)) 'projects-current)
      projects--current))

(defun projects--set-current (name &optional frame-only)
  "Set the active project to NAME on the selected frame.
Also updates the global `projects--current' unless FRAME-ONLY is non-nil."
  (set-frame-parameter nil 'projects-current name)
  (unless frame-only
    (setq projects--current name)))

(defun projects-view-mode (&optional frame)
  (or (frame-parameter (or frame (selected-frame)) 'projects-view-mode)
      'single-project))

(defun projects-multi-project-view-p (&optional frame)
  (eq (projects-view-mode frame) 'multi-project))

(defun projects-current-window-project (&optional window)
  (let ((win (or window (selected-window))))
    (or (window-parameter win 'projects-project)
        (projects-current (window-frame win)))))

(defun projects--set-window-project (window project)
  (set-window-parameter window 'projects-project project))

(defun projects--set-view-mode (mode &optional frame)
  (set-frame-parameter (or frame (selected-frame)) 'projects-view-mode mode))

(defun projects--set-multi-layout (layout &optional frame)
  (set-frame-parameter (or frame (selected-frame)) 'projects-multi-layout layout))

(defun projects--clear-window-project-params ()
  "Clear projects-project window parameter from all windows in the selected frame.
Must be called when leaving multi-project mode to prevent stale parameters
from persisting into single-project mode."
  (dolist (win (window-list nil 0))
    (set-window-parameter win 'projects-project nil)))

(defun projects-enter-single-project-view ()
  (interactive)
  (let ((project (projects-current-window-project)))
    (projects--set-view-mode 'single-project)
    (set-frame-parameter nil 'projects-multi-layout nil)
    ;; Clear window project assignments — these are multi-project-mode-only
    (projects--clear-window-project-params)
    ;; Clear per-buffer header-line-format set during multi-project mode
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (equal header-line-format '(:eval (projects--window-header-line)))
          (kill-local-variable 'header-line-format))))
    (force-mode-line-update t)
    (projects--update-frame-tab-bar)
    (when project
      (projects-switch project))))

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
  (cl-sort (copy-sequence (projects-names))
           (lambda (a b)
             (let ((ta (or (plist-get (gethash a projects--table) :switch-time) 0))
                   (tb (or (plist-get (gethash b projects--table) :switch-time) 0)))
               (> ta tb)))))

(defun projects-hidden-p (name)
  "Return t if project NAME is hidden (not shown in UI lists or tab-bar)."
  (plist-get (gethash name projects--table) :hidden))

(defun projects-names-visible ()
  "Return non-hidden project names, MRU-sorted."
  (cl-remove-if #'projects-hidden-p (projects-names-mru)))

(defun projects-special-buffer-p (buf)
  "Return t if BUF is a global/special buffer that should appear in all projects."
  (let ((name (buffer-name buf)))
    (cl-some (lambda (pattern) (string-match-p pattern name))
             projects--special-buffer-patterns)))

(defun projects--pinned-buffer-p (buf)
  "Return t if BUF is an interactive special buffer that must not be displaced.
Unlike `projects-special-buffer-p', this excludes fallback buffers like *scratch*
and *Messages* that should be replaced by project content after a buffer kill."
  (let ((name (buffer-name buf)))
    (cl-some (lambda (pattern) (string-match-p pattern name))
             '("^\\*vterm" "^\\*eat" "^magit" "^COMMIT_EDITMSG$"))))

;;; ---------------------------------------------------------------------------
;;; CRUD
;;; ---------------------------------------------------------------------------

(defun projects--unique-project-name (base)
  "Return BASE if no project with that name exists, else BASE [1], BASE [2], …"
  (if (not (gethash base projects--table))
      base
    (let ((n 1))
      (while (gethash (format "%s [%d]" base n) projects--table)
        (setq n (1+ n)))
      (format "%s [%d]" base n))))

(defun projects-create (name dir)
  "Create a new project named NAME with root directory DIR.
NAME must be unique. DIR is created if it does not exist."
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
  (puthash name (list :dir (file-name-as-directory (expand-file-name dir))
                      :buffers nil
                      :files nil
                      :switch-time 0)
           projects--table)
  (message "[projects] create: %s dir=%s" name dir)
  (projects-switch name)
  ;; In multi-project mode, also assign the new project to the current window.
  (when (projects-multi-project-view-p)
    (projects--set-window-project (selected-window) name)
    (switch-to-buffer (projects--window-buffer-for-project name))
    (projects--refresh-window-project-headers))
  name)

(defun projects--repo-name-from-url (url)
  "Extract the repository name from URL or GitHub shorthand ORG/REPO.
Strips a trailing .git suffix if present."
  (let ((name (if (string-match "/\\([^/]+\\)/?$" url)
                  (match-string 1 url)
                url)))
    (if (string-suffix-p ".git" name)
        (substring name 0 -4)
      name)))

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
  (when (or (null repo-url) (string-empty-p repo-url))
    (user-error "Repository URL must not be empty"))
  (when (or (null project-name) (string-empty-p project-name))
    (user-error "Project name must not be empty"))
  (when (or (file-name-absolute-p project-name)
            (string-match-p "\\.\\." project-name)
            (string-match-p "/" project-name))
    (user-error "Project name must be a simple name (no path separators or '..')"))
  (when (gethash project-name projects--table)
    (user-error "Project '%s' already exists" project-name))
  (let* ((target (expand-file-name project-name root-dir))
         (github-p (string-match-p "^[A-Za-z0-9][A-Za-z0-9_.-]*/[A-Za-z0-9][A-Za-z0-9_.-]*$" repo-url))
         (cmd  (if github-p
                   (list "gh" "repo" "clone" repo-url target)
                 (list "git" "clone" repo-url target))))
    (when (and github-p (not (executable-find "gh")))
      (user-error "gh CLI not found — install it or use a full URL"))
    (when (file-exists-p target)
      (user-error "Directory '%s' already exists" target))
    (message "[projects] cloning %s into %s..." repo-url target)
    (let ((buf (get-buffer-create "*projects-clone*")))
      (with-current-buffer buf (erase-buffer))
      (let ((exit-code (apply #'call-process (car cmd) nil (list buf t) nil (cdr cmd))))
        (if (eq exit-code 0)
            (progn
              (message "[projects] clone succeeded")
              (unless (file-directory-p target)
                (user-error "Clone reported success but target directory '%s' was not created — see *projects-clone* buffer" target))
              (projects-create project-name target))
          (user-error "Clone failed (exit %s) — see *projects-clone* buffer"
                      exit-code))))))

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
  ;; Update current project reference across all frames and global
  (dolist (f (frame-list))
    (when (equal (frame-parameter f 'projects-current) old-name)
      (set-frame-parameter f 'projects-current new-name)))
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
    ;; Switch away from the deleted project in any frame that had it active
    (dolist (f (frame-list))
      (when (equal (frame-parameter f 'projects-current) name)
        (let ((others (projects-names-visible)))
          (if others
              (with-selected-frame f (projects-switch (car others)))
            (set-frame-parameter f 'projects-current nil)))))
    (when (equal projects--current name)
      (setq projects--current nil))
    (message "[projects] delete: %s" name)))

;;; ---------------------------------------------------------------------------
;;; Switching
;;; ---------------------------------------------------------------------------

(defvar projects-switch-hook nil
  "Hook run after switching to a project.")

(defun projects-switch (name &optional norecord)
  "Switch to project NAME. Updates tab-bar and active buffers."
  (interactive
   (progn
     (when (projects-hidden-p (projects-current))
       (user-error "Cannot switch projects from a temporary project"))
     (let ((candidates (cl-remove (projects-current) (projects-names-visible) :test #'equal)))
       (list (completing-read "Switch to project: "
                              (lambda (str pred action)
                                (if (eq action 'metadata)
                                    '(metadata (display-sort-function . identity))
                                  (complete-with-action action candidates str pred)))
                              nil t)))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  ;; Block interactive switching to/from hidden projects
  (when (called-interactively-p 'any)
    (when (projects-hidden-p (projects-current))
      (user-error "Cannot switch projects from a temporary project"))
    (when (projects-hidden-p name)
      (user-error "Cannot switch to a hidden project")))
  (message "[projects] switch: %s -> %s%s (multi=%s caller: %s)"
           (projects-current) name (if norecord " (norecord)" "")
           (projects-multi-project-view-p)
           (or (ignore-errors (cadr (backtrace-frame 2))) "?"))
  (if (projects-multi-project-view-p)
      ;; In multi-project mode: only update frame-level tracking, never touch layout.
      ;; Window assignments are managed per-window; projects-switch must not clobber them.
      (progn
        (message "[projects] switch (multi-project mode): updating frame tracking only, not touching windows")
        (projects--set-current name)
        (unless norecord
          (let ((proj (gethash name projects--table)))
            (plist-put proj :switch-time (float-time))
            (puthash name proj projects--table)))
        (unless (projects-hidden-p name)
          (when-let ((dir (projects-dir name)))
            (setq-default default-directory dir)))
        (run-hooks 'projects-switch-hook))
    ;; Single-project mode: full switch with window config save/restore.
    ;; Save current window layout for the outgoing project (before anything changes)
    (let ((old (projects-current)))
      (when (and old (not (projects-hidden-p old)))
        (let ((entry (gethash old projects--table)))
          (when entry
            (plist-put entry :window-config (current-window-configuration))
            (puthash old entry projects--table)))))
    (projects--set-current name)
    (unless norecord
      (let ((proj (gethash name projects--table)))
        (plist-put proj :switch-time (float-time))
        (puthash name proj projects--table)))
    (unless (projects-hidden-p name)
      (when-let ((dir (projects-dir name)))
        (setq-default default-directory dir)))
    (projects--update-frame-tab-bar)
    (let* ((entry  (gethash name projects--table))
           (config (and entry (plist-get entry :window-config))))
      (if config
          (progn
            (set-window-configuration config)
            ;; A restored config may contain stale projects-project window params
            ;; from a previous multi-project session. Clear them — they are only
            ;; meaningful in multi-project mode.
            (projects--clear-window-project-params))
        (projects--ensure-visible-buffer)))
    (projects--tab-bar-refresh)
    (run-hooks 'projects-switch-hook)))

(defconst projects--multi-layouts '("2x1" "2x2" "3x2")
  "Available multi-project layout names, in display order.")

(defun projects--valid-multi-layout-p (layout)
  (member layout projects--multi-layouts))

(defun projects--layout-window-count (layout)
  "Return the number of windows for LAYOUT string (e.g. \"2x1\" → 2)."
  (pcase layout
    ("2x1" 2)
    ("2x2" 4)
    ("3x2" 6)
    (_ 2)))

(defun projects--read-multi-layout (&optional prompt)
  (let* ((choices (mapcar (lambda (l)
                            (format "%s (%d)" l (projects--layout-window-count l)))
                          projects--multi-layouts))
         (result (completing-read (or prompt "Multi-project layout: ")
                                  choices nil t nil nil
                                  (format "2x1 (%d)" (projects--layout-window-count "2x1")))))
    (car (split-string result " "))))

(defun projects--refresh-window-project-headers ()
  (dolist (win (window-list nil 0))
    (with-current-buffer (window-buffer win)
      (setq header-line-format
            (when (projects-multi-project-view-p (window-frame win))
              '(:eval (projects--window-header-line))))))
  (force-mode-line-update t))

(defun projects--window-project-seed-list ()
  (let* ((current (projects-current))
         (visible (projects-names-visible))
         (ordered (if (and current (member current visible))
                      (cons current (cl-remove current visible :test #'equal))
                    visible)))
    (or ordered (and current (list current)))))

(defun projects--fill-project-list (projects count)
  (when (and projects (> (length projects) 0))
    (let ((seed (copy-sequence projects))
          (result nil)
          (index 0))
      (dotimes (_ count)
        (let ((project (nth (mod index (length seed)) seed)))
          (push project result)
          (setq index (1+ index))))
      (nreverse result))))

(defun projects--split-for-layout (layout)
  (delete-other-windows)
  (pcase layout
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
(defun projects--window-buffer-for-project (project)
  (or (cl-find-if (lambda (buffer)
                    (equal (buffer-local-value 'projects--buffer-project buffer) project))
                  (buffer-list))
      (projects--create-info-buffer project)))

(defun projects--apply-multi-project-layout (layout &optional projects)
  "Split the frame into LAYOUT windows and assign projects to each.
PROJECTS is an optional list of project names to assign; defaults to visible projects."
  (let* ((wins nil)
         (seed (or projects (projects--window-project-seed-list)))
         (assignments (projects--fill-project-list seed (projects--layout-window-count layout))))
    (when assignments
      (projects--split-for-layout layout)
      (setq wins (window-list nil 0))
      (cl-mapc (lambda (win project)
                 (projects--set-window-project win project)
                 (with-selected-window win
                   (switch-to-buffer (projects--window-buffer-for-project project))))
               wins assignments)
      (select-window (car wins)))))

(defun projects-enter-multi-project-view (layout)
  (interactive (list (projects--read-multi-layout)))
  (unless (projects--valid-multi-layout-p layout)
    (user-error "Unsupported layout: %s" layout))
  (projects--set-view-mode 'multi-project)
  (projects--set-multi-layout layout)
  (projects--apply-multi-project-layout layout)
  (projects--refresh-window-project-headers)
  (projects--update-frame-tab-bar)
  (projects--tab-bar-refresh))

(defun projects-switch-window-project (name)
  (interactive
   (list (completing-read "Switch window project: " (projects-names-visible) nil t)))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (projects--set-window-project (selected-window) name)
  (switch-to-buffer (projects--window-buffer-for-project name))
  (projects--refresh-window-project-headers))

(defun projects-switch-dispatch ()
  (interactive)
  (if (projects-multi-project-view-p)
      (call-interactively #'projects-switch-window-project)
    (call-interactively #'projects-switch)))

(defun projects-change-multi-project-layout (layout)
  (interactive (list (projects--read-multi-layout "Change multi-project layout: ")))
  (unless (projects-multi-project-view-p)
    (user-error "Not in multi-project view"))
  (projects--set-multi-layout layout)
  (projects--apply-multi-project-layout
   layout
   (mapcar (lambda (win) (window-parameter win 'projects-project))
           (window-list nil 0)))
  (projects--refresh-window-project-headers))

;;; ---------------------------------------------------------------------------
;;; Buffer Management
;;; ---------------------------------------------------------------------------

(defun projects-register-buffer (buf &optional project-name)
  "Register BUF as belonging to PROJECT-NAME or the active project scope."
  (when (buffer-live-p buf)
    (let ((proj (or project-name
                    (if (projects-multi-project-view-p)
                        (projects-current-window-project)
                      (projects-current)))))
      (message "[projects] register-buffer: buf=%s proj=%s explicit=%s multi=%s sel-win=%s win-proj=%s"
               (buffer-name buf) proj (not (null project-name)) (projects-multi-project-view-p)
               (selected-window) (window-parameter (selected-window) 'projects-project))
      (when (and proj (not (projects-special-buffer-p buf)))
        (with-current-buffer buf
          (setq-local projects--buffer-project proj))
        (let* ((entry (gethash proj projects--table))
               (bufs (plist-get entry :buffers)))
          (unless (memq buf bufs)
            (plist-put entry :buffers (cons buf bufs))
            (puthash proj entry projects--table)))))))

(defun projects--ensure-tmp-project ()
  "Create the hidden 'tmp' project if it doesn't exist yet."
  (unless (gethash "tmp" projects--table)
    (puthash "tmp" (list :dir (expand-file-name "~/")
                         :buffers nil
                         :files nil
                         :switch-time 0
                         :hidden t)
             projects--table)))

(defun projects--find-file-hook ()
  "Register newly opened files with the current project.
Only truly fresh client frames (client flag set, no project yet) route
to tmp. Interactive daemon sessions that already have a project behave
like any normal frame."
  (let ((buf (current-buffer))
        (sel (selected-window))
        (win-proj (window-parameter (selected-window) 'projects-project))
        (multi (projects-multi-project-view-p)))
    (message "[projects] find-file-hook: buf=%s selected-win=%s win-proj=%s multi=%s frame-proj=%s"
             (buffer-name buf) sel win-proj multi (projects-current))
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
     ;; Multi-project mode with a known window project: register immediately so
     ;; that any concurrent buffer-kill / fix-windows-after-kill sees the buffer
     ;; as belonging here.  Without a known win-proj we defer to
     ;; window-buffer-change-hook as before.
     ((and multi win-proj)
      (message "[projects] find-file-hook: registering buf=%s to win-proj=%s (multi, immediate)"
               (buffer-name buf) win-proj)
      (projects-register-buffer buf win-proj))
     (multi
      (message "[projects] find-file-hook: deferring registration to window-buffer-change-hook (multi-project, no win-proj)"))
     ;; Normal single-project case
     (t
      (projects-register-buffer buf)))))

(defun projects--window-buffer-change-hook (frame)
  "In multi-project mode, re-register buffers appearing in windows with the
correct window project, and refresh the header-line-format."
  (when (projects-multi-project-view-p frame)
    (let ((refreshed nil))
      (dolist (win (window-list frame 0))
        (let* ((buf      (window-buffer win))
               (win-proj (window-parameter win 'projects-project))
               (buf-proj (buffer-local-value 'projects--buffer-project buf)))
          (when win-proj
            ;; Only register buffers that have no project yet (buf-proj=nil).
            ;; Buffers already assigned to a project keep their assignment
            ;; regardless of which window displays them — use projects-move-buffer
            ;; for intentional reassignment. This prevents vertico-buffer-mode
            ;; and claude-display from re-homing buffers to the wrong project.
            (when (and (not (projects-special-buffer-p buf))
                       (not (string-match-p "^\\*project: " (buffer-name buf)))
                       (null buf-proj))
              (message "[projects] window-buffer-change: win=%s buf=%s old-proj=nil new-proj=%s"
                       win (buffer-name buf) win-proj)
              (projects-register-buffer buf win-proj))
            ;; Ensure header-line-format is set for every buffer in a project window
            (with-current-buffer buf
              (unless (equal header-line-format '(:eval (projects--window-header-line)))
                (setq header-line-format '(:eval (projects--window-header-line)))
                (setq refreshed t))))))
      (when refreshed
        (force-mode-line-update t)))))

(defun projects--cleanup-dead-buffers ()
  "Remove the current (dying) buffer from its owning project's buffer list."
  (let ((proj projects--buffer-project)
        (buf (current-buffer)))
    (when proj
      (let* ((entry (gethash proj projects--table))
             (bufs (plist-get entry :buffers)))
        (when (memq buf bufs)
          (message "[projects] buffer-killed: %s from project %s" (buffer-name buf) proj)
          (plist-put entry :buffers (delq buf bufs))
          (puthash proj entry projects--table)
          ;; After the kill, ensure all windows stay within their project
          (when (or (equal proj (projects-current))
                    (projects-multi-project-view-p))
            (run-with-timer 0 nil #'projects--fix-windows-after-kill)))))))

(defun projects--window-target-project (window)
  (if (projects-multi-project-view-p (window-frame window))
      (projects-current-window-project window)
    (projects-current (window-frame window))))

(defun projects--fix-windows-after-kill ()
  "Ensure no window shows a non-project buffer after a project buffer is killed.
In multi-project mode each window's assigned project is used; in single-project
mode the frame-wide current project is used."
  (dolist (win (window-list nil 0))
    (let* ((proj (projects--window-target-project win))
           (buf (window-buffer win))
           (bname (buffer-name buf))
           (info-buf-name (when proj (projects--info-buffer-name proj)))
           (proj-bufs (when proj (plist-get (gethash proj projects--table) :buffers))))
      (message "[projects] fix-windows: win=%s proj=%s buf=%s info=%s pinned=%s in-proj=%s"
               win proj bname info-buf-name
               (projects--pinned-buffer-p buf)
               (and proj-bufs (memq buf proj-bufs) t))
      (unless (or (null proj)
                  (string= bname info-buf-name)
                  (projects--pinned-buffer-p buf)
                  (memq buf proj-bufs))
        (let ((next (cl-find-if
                     (lambda (b)
                       (and (buffer-live-p b)
                            (not (eq b buf))
                            (memq b proj-bufs)))
                     (buffer-list))))
          (message "[projects] fix-windows: switching win=%s to %s (next=%s)"
                   win (or (and next (buffer-name next)) (concat "*project: " proj "*")) next)
          (with-selected-window win
            (switch-to-buffer (or next (projects--create-info-buffer proj)))))))))

(defun projects-inspect (name)
  "Show directory and buffer list for project NAME in *Messages*."
  (interactive
   (list (completing-read "Inspect project: " (projects-names) nil t
                          nil nil (or (projects-current-window-project) (projects-current)))))
  (let ((dir   (projects-dir name))
        (bufs  (projects-buffers name))
        (entry (gethash name projects--table)))
    (message "[projects] %s  dir=%s  buffers=(%s)  hidden=%s"
             name dir
             (mapconcat #'buffer-name bufs " ")
             (projects-hidden-p name))))

(defun projects-switch-buffer ()
  "Switch to a buffer belonging to the active project scope."
  (interactive)
  (let* ((multi (projects-multi-project-view-p))
         (proj (if multi
                   (projects-current-window-project)
                 (projects-current)))
         (all (buffer-list))
         (project-bufs (when proj
                         (cl-remove-if-not
                          #'buffer-live-p
                          (plist-get (gethash proj projects--table) :buffers))))
         (special-bufs (cl-remove-if-not #'projects-special-buffer-p all))
         (ordered (append project-bufs special-bufs))
         (names (mapcar #'buffer-name ordered)))
    (switch-to-buffer
     (completing-read (format "Buffer [%s]: " (or proj "global"))
                      names nil t))))

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

;;; ---------------------------------------------------------------------------
;;; Info Buffer Mode
;;; ---------------------------------------------------------------------------

(defun projects-info-open-directory ()
  "Open Dired in the current project's root directory."
  (interactive)
  (dired default-directory))

(defun projects-info-open-magit ()
  "Open Magit using the existing global project shortcut."
  (interactive)
  (call-interactively (key-binding (kbd "C-c p v"))))

(defun projects-info-open-vterm ()
  "Open a vterm in the current project's root directory."
  (interactive)
  (+vterm/here nil))

(defvar projects-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'claude-code)
    (define-key map (kbd "c") #'claude-code-continue)
    (define-key map (kbd "r") #'claude-code-resume)
    (define-key map (kbd "d") #'projects-info-open-directory)
    (define-key map (kbd "g") #'projects-info-open-magit)
    (define-key map (kbd "v") #'projects-info-open-vterm)
    map)
  "Keymap for `projects-info-mode'.")

(define-derived-mode projects-info-mode special-mode "Project Info"
  "Major mode for the project info buffer.
Provides single-key shortcuts to launch Claude Code sessions."
  :interactive nil)

;; Put this buffer in Evil emacs state so n/c/r are not shadowed by Vim bindings
(after! evil
  (evil-set-initial-state 'projects-info-mode 'emacs))

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
        (setq-local default-directory dir)
        (insert "\n\n")
        (insert (propertize (format "  Project: %s\n" project-name)
                            'face '(:weight bold :height 1.4)))
        (insert (propertize (format "  Directory: %s\n" (abbreviate-file-name dir))
                            'face 'font-lock-comment-face))
        (insert (propertize (format "  Buffers: %d\n" buf-count)
                            'face 'font-lock-comment-face))
        (insert "\n")
        (insert (propertize "  Open a file with C-x C-f to get started.\n"
                            'face 'font-lock-doc-face))
        (insert "\n")
        (insert (propertize "  Claude Code:\n" 'face '(:weight bold)))
        (insert (propertize "    n" 'face 'font-lock-keyword-face))
        (insert "  new session  ")
        (insert (propertize "  c" 'face 'font-lock-keyword-face))
        (insert "  continue  ")
        (insert (propertize "  r" 'face 'font-lock-keyword-face))
        (insert "  resume\n")
        (insert "\n")
        (insert (propertize "  Project:\n" 'face '(:weight bold)))
        (insert (propertize "    d" 'face 'font-lock-keyword-face))
        (insert "  open dir  ")
        (insert (propertize "     g" 'face 'font-lock-keyword-face))
        (insert "  magit  ")
        (insert (propertize "     v" 'face 'font-lock-keyword-face))
        (insert "  vterm\n"))
      (unless (derived-mode-p 'projects-info-mode)
        (projects-info-mode))
      (goto-char (point-min)))
    buf))

(defun projects-show-info ()
  "Show the info buffer for the current project."
  (interactive)
  (if projects--current
      (switch-to-buffer (projects--create-info-buffer projects--current))
    (user-error "No active project")))

(defun projects--ensure-visible-buffer ()
  "Ensure ALL visible windows show buffers belonging to the current project.
Windows showing buffers from other projects are replaced.  Multiple windows
each receive a different project buffer (pool approach) so the same buffer
is not duplicated across windows."
  (let* ((proj (projects-current))
         (info-buf-name (when proj (projects--info-buffer-name proj))))
    (when proj
      (let* ((all-wins (window-list nil 0))
             ;; A window needs fixing if it shows: a foreign project's buffer,
             ;; OR another project's info buffer (even though it starts with *).
             (needs-fix
              (cl-remove-if
               (lambda (win)
                 (let* ((buf  (window-buffer win))
                        (bname (buffer-name buf))
                        (bproj (buffer-local-value 'projects--buffer-project buf)))
                   (or (string= bname info-buf-name)   ; already this project's info
                       (equal bproj proj)              ; already this project's buffer
                       ;; special buffer that is NOT another project's info
                       (and (projects-special-buffer-p buf)
                            (not (string-match-p "^\\*project: " bname))))))
               all-wins))
             ;; Buffers already visible in windows we're keeping — exclude from pool
             (kept-bufs (mapcar #'window-buffer
                                (cl-set-difference all-wins needs-fix)))
             ;; Pool: project buffers not already pinned in a kept window, MRU order
             (pool (cl-remove-if
                    (lambda (b)
                      (or (not (buffer-live-p b))
                          (memq b kept-bufs)
                          (not (equal (buffer-local-value 'projects--buffer-project b)
                                      proj))))
                    (buffer-list))))
        (dolist (win needs-fix)
          (let ((replacement (or (pop pool) (projects--create-info-buffer proj))))
            (with-selected-window win
              (switch-to-buffer replacement))))))))

;;; ---------------------------------------------------------------------------
;;; ibuffer Integration
;;; ---------------------------------------------------------------------------

(defun projects-ibuffer-groups ()
  "Return ibuffer filter groups for all current projects.
Groups file buffers by project directory using the built-in filename filter.
Buffers not under any project directory fall into 'Other'."
  (append
   (mapcar (lambda (name)
            (let* ((dir (projects-dir name))
                   (info-name (regexp-quote (projects--info-buffer-name name)))
                   (reg-names (mapcar (lambda (b) (regexp-quote (buffer-name b)))
                                      (projects-buffers name)))
                   (name-rx (mapconcat #'identity
                                       (cons info-name reg-names) "\\|")))
              (list name `(or (filename . ,(expand-file-name (or dir "")))
                              (name . ,name-rx)))))
          (projects-names-mru))
   '(("Other" (name . ".*")))))

(defun projects--ibuffer-setup ()
  "Set up ibuffer to use project groups."
  (setq ibuffer-filter-groups (projects-ibuffer-groups))
  (ibuffer-update nil t))

;;; ---------------------------------------------------------------------------
;;; Tab-bar Integration
;;; ---------------------------------------------------------------------------

(defvar projects--focused-window nil
  "The window with keyboard focus, captured by post-command-hook.
Used by header-line rendering where selected-window is temporarily rebound.")

(defun projects--track-focused-window ()
  "Update `projects--focused-window' and trigger header-line refresh on focus change."
  (unless (eq projects--focused-window (selected-window))
    (setq projects--focused-window (selected-window))
    (force-mode-line-update t)))

(defun projects--window-header-line ()
  ;; Compare the rendering window (selected-window during :eval) against the
  ;; keyboard-focused window captured outside of redisplay.
  (let* ((project  (projects-current-window-project))
         (selected (eq (selected-window) projects--focused-window))
         ;; Use active bg for selected window, inactive bg otherwise.
         (bg      (if selected
                      (face-background 'my/workspace-tab-active nil t)
                    (face-background 'my/workspace-tab-inactive nil t)))
         ;; Use vertical-border fg, then bg, then fallback — same color as window dividers.
         (divider (or (face-foreground 'vertical-border nil t)
                      (face-background 'vertical-border nil t)
                      "gray30"))
         ;; Plain color string: most compatible underline form; renders reliably in header-line.
         (text-face (if selected
                        '(:inherit my/workspace-tab-active)
                      '(:inherit my/workspace-tab-inactive)))
         (text   (propertize (format " %s " (or project "no project")) 'face text-face))
         ;; Filler: bg + underline for inactive only, stretches to right edge.
         (filler (propertize " " 'face (if selected
                                          `(:background ,bg)
                                        `(:background ,bg :underline ,divider))
                             'display '(space :align-to right))))
    (concat text filler)))

(defun projects--tab-bar-format ()
  "Tab-bar format function: renders all projects MRU-sorted with active one highlighted.
Reuses faces my/workspace-tab-active and my/workspace-tab-inactive from +functions.el."
  (let* ((current (projects-current))
         (visible (projects-names-visible))
         ;; Always show active project first, even if hidden (so user knows where they are)
         (names (if (and current (not (member current visible)))
                    (cons current visible)
                  (if current
                      (cons current (cl-remove current visible :test #'equal))
                    visible))))
    (mapcar
     (lambda (name)
       (let* ((face (if (equal name current)
                        'my/workspace-tab-active
                      'my/workspace-tab-inactive))
              (key-sym (intern (concat "proj-" name)))
              (captured-name name)
              (label (propertize (format " %s " name)
                                 'face face
                                 'tab-bar-key key-sym)))
         `(,key-sym
           menu-item
           ,label
           (lambda () (interactive) (projects-switch ,captured-name))
           :help ,(format "Switch to project: %s → %s"
                          name
                          (abbreviate-file-name (or (projects-dir name) ""))))))
     names)))

(defun projects--tab-bar-refresh (&rest _)
  "Force tab-bar redraw after project state changes."
  (when (bound-and-true-p tab-bar-mode)
    (force-mode-line-update t)))

(defun projects--update-frame-tab-bar (&optional frame)
  "Show or hide the tab-bar for FRAME based on whether its project is hidden or in multi-project view."
  (let* ((f    (or frame (selected-frame)))
         (proj (projects-current f))
         (multi (projects-multi-project-view-p f))
         (hide  (or multi (and proj (projects-hidden-p proj)))))
    (message "[projects] update-tab-bar: proj=%s multi=%s hidden=%s -> tab-bar=%s (caller: %s)"
             proj multi (and proj (projects-hidden-p proj))
             (if hide 0 1)
             (or (ignore-errors (cadr (backtrace-frame 1))) "?"))
    (set-frame-parameter f 'tab-bar-lines (if hide 0 1))))

;;; ---------------------------------------------------------------------------
;;; Persistence
;;; ---------------------------------------------------------------------------

(defun projects--rotate-backups ()
  "Rotate session backups before saving: .4 dropped, .3→.4, .2→.3, .1→.2, current→.1."
  (when (file-exists-p projects--save-file)
    ;; Drop oldest backup
    (let ((oldest (format "%s.%d" projects--save-file projects--backup-count)))
      (when (file-exists-p oldest) (delete-file oldest)))
    ;; Shift existing backups up by one
    (cl-loop for n from (1- projects--backup-count) downto 1
             for src = (if (= n 1) projects--save-file
                         (format "%s.%d" projects--save-file (1- n)))
             for dst = (format "%s.%d" projects--save-file n)
             when (file-exists-p src)
             do (rename-file src dst t))))

(defun projects-save ()
  "Save project state to `projects--save-file'.
Rotates up to `projects--backup-count' backups before writing."
  (interactive)
  (make-directory (file-name-directory projects--save-file) t)
  (projects--rotate-backups)
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
          ;; Don't persist hidden projects (they're recreated on demand)
          (cl-remove-if #'projects-hidden-p (projects-names)))))
    (with-temp-file projects--save-file
      (let ((print-level nil)
            (print-length nil))
        (pp (list :version 1
                  :current projects--current
                  :projects data
                  :view-mode (projects-view-mode)
                  :multi-layout (frame-parameter nil 'projects-multi-layout)
                  :window-projects (mapcar (lambda (win)
                                             (window-parameter win 'projects-project))
                                           (window-list nil 0)))
            (current-buffer)))))
  (let ((inhibit-message t))
    (message "Projects saved")))

(defun projects--format-relative-age (mtime)
  "Return a short relative age string for MTIME."
  (let* ((seconds (max 0 (floor (float-time (time-subtract (current-time) mtime))))))
    (cond
     ((< seconds 60) (format "%ss ago" seconds))
     ((< seconds 3600) (format "%sm ago" (/ seconds 60)))
     ((< seconds 86400) (format "%sh ago" (/ seconds 3600)))
     (t (format "%sd ago" (/ seconds 86400))))))

(defun projects--backup-label (base-label path)
  "Return chooser label for BASE-LABEL and PATH with timestamp details."
  (let* ((attrs (file-attributes path))
         (mtime (file-attribute-modification-time attrs))
         (absolute (format-time-string "%Y-%m-%d %H:%M" mtime))
         (relative (projects--format-relative-age mtime)))
    (format "%s — %s — %s" base-label absolute relative)))

(defun projects--backup-files ()
  "Return existing backup files as an alist of (label . path), newest first."
  (let (result)
    (when (file-exists-p projects--save-file)
      (push (cons (projects--backup-label "current  (session.el)" projects--save-file)
                  projects--save-file)
            result))
    (cl-loop for n from 1 to projects--backup-count
             for path = (format "%s.%d" projects--save-file n)
             when (file-exists-p path)
             do (push (cons (projects--backup-label
                             (format "backup %d (session.el.%d)" n n)
                             path)
                            path)
                      result))
    (nreverse result)))

(defun projects-restore (&optional file)
  "Restore project state from FILE (default `projects--save-file').
With prefix arg \\[universal-argument], prompt to choose from available backups."
  (interactive
   (list (if current-prefix-arg
             (let* ((choices (projects--backup-files))
                    (labels  (mapcar #'car choices)))
               (when (null choices)
                 (user-error "No session files found"))
               (cdr (assoc (completing-read "Restore from: " labels nil t)
                           choices)))
           projects--save-file)))
  (let ((file (or file projects--save-file)))
  (if (not (file-exists-p file))
      (message "No saved projects session found")
    (let* ((data (with-temp-buffer
                   (insert-file-contents file)
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
          (sit-for 0)

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

          ;; Restore files for each project (disable find-file-hook to avoid double registration)
          (let ((find-file-hook nil))
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
                            (when (and (local-variable-p 'progress-start-marker)
                                       (local-variable-p 'progress-end-marker))
                              (delete-region progress-start-marker progress-end-marker)
                              (goto-char progress-start-marker)
                              (insert (propertize (format "%d%% (%d/%d)" pct current-count total)
                                                  'face `(:height 1.2 :foreground ,green-color)))
                              (set-marker progress-end-marker (point))))))
                      (sit-for 0))))))) ; end let find-file-hook / dolist project-list

          ;; Restore active project (global + current frame)
          (let ((restored (if (and saved-current (gethash saved-current projects--table))
                              saved-current
                            (car (projects-names-mru)))))
            (setq projects--current restored)
            (set-frame-parameter nil 'projects-current restored))

          ;; Restore view mode
          (let* ((saved-view-mode (plist-get data :view-mode))
                 (saved-multi-layout (plist-get data :multi-layout))
                 (saved-window-projects (plist-get data :window-projects)))
            (if (eq saved-view-mode 'multi-project)
                (progn
                  (projects--set-view-mode 'multi-project)
                  (projects--set-multi-layout saved-multi-layout)
                  (when saved-window-projects
                    (projects--apply-multi-project-layout saved-multi-layout saved-window-projects)))
              (projects--set-view-mode 'single-project))
            (projects--refresh-window-project-headers)
            (projects--update-frame-tab-bar))

          ;; Clean up and show dashboard (deferred so progress renders)
          (run-with-timer 0.2 nil
                          (lambda ()
                            (when (get-buffer "*session-loading*")
                              (kill-buffer "*session-loading*"))
                            (when (fboundp 'projects--tab-bar-refresh)
                              (projects--tab-bar-refresh))
                            (when (fboundp '+doom-dashboard-reload)
                              (+doom-dashboard-reload t))))))))))

;;; ---------------------------------------------------------------------------
;;; Setup Hooks
;;; ---------------------------------------------------------------------------

(defun projects--auto-switch-on-display (frame)
  "Auto-switch project when the selected window shows a buffer from a different project.
Fires on `window-buffer-change-functions' — handles the case where killing
a buffer causes Emacs to show a buffer from another project.
Updates only the per-frame project; does not affect other frames."
  (let* ((buf          (window-buffer (frame-selected-window frame)))
         (buf-proj     (buffer-local-value 'projects--buffer-project buf))
         (frame-proj   (projects-current frame)))
    (when (and buf-proj
               (not (equal buf-proj frame-proj))
               (gethash buf-proj projects--table)
               ;; Never auto-switch into or out of a hidden project
               (not (projects-hidden-p buf-proj))
               (not (projects-hidden-p frame-proj)))
      (message "[projects] auto-switch %s -> %s (frame, triggered by buffer: %s)"
               frame-proj buf-proj (buffer-name buf))
      ;; Update only this frame — other frames keep their own current project
      (set-frame-parameter frame 'projects-current buf-proj)
      ;; Update global for non-client frames (regular project auto-switches)
      (unless (frame-parameter frame 'client)
        (setq projects--current buf-proj))
      ;; Only update global default-directory for non-client, non-hidden project switches
      (unless (or (frame-parameter frame 'client) (projects-hidden-p buf-proj))
        (when-let ((dir (projects-dir buf-proj)))
          (setq-default default-directory dir)))
      ;; Show/hide tab-bar for this frame based on the new project
      (projects--update-frame-tab-bar frame)
      ;; Defer refresh — force-mode-line-update inside window-buffer-change-functions
      ;; is deferred until next event loop; a timer fires after current cycle completes.
      (run-with-timer 0 nil #'projects--tab-bar-refresh))))

(defun projects--set-window-project-dir (&rest _)
  "Set `default-directory' to the current window's project root.
Used as :before advice on vterm/eat so the terminal opens in the right dir."
  (when (projects-multi-project-view-p)
    (when-let* ((proj (projects-current-window-project))
                (dir  (projects-dir proj)))
      (message "[projects] set-window-project-dir: %s -> %s" proj dir)
      (setq default-directory dir))))

(defvar projects--hooks-installed-p nil
  "Non-nil if projects hooks have already been installed.")

(defun projects--setup-hooks ()
  "Install all hooks for buffer tracking, auto-save, and frame management.
Idempotent: safe to call multiple times."
  (unless projects--hooks-installed-p
    (setq projects--hooks-installed-p t)
    (add-hook 'find-file-hook #'projects--find-file-hook)
    (add-hook 'kill-buffer-hook #'projects--cleanup-dead-buffers)
    ;; Register terminal and dired buffers with the current project on creation.
    ;; For vterm/eat: also set default-directory to the window project's root
    ;; before the terminal opens, since the buffer is created with whatever
    ;; default-directory was current — the mode-hook fires too late to change it.
    (advice-add 'vterm :before #'projects--set-window-project-dir)
    (advice-add 'vterm-other-window :before #'projects--set-window-project-dir)
    (with-eval-after-load 'eat
      (advice-add 'eat :before #'projects--set-window-project-dir))
    ;; claude-code--directory uses (project-root (project-current)) which walks
    ;; up to the git root — ignoring default-directory. In multi-view mode,
    ;; override it to return the window project's registered dir directly.
    (advice-add 'claude-code--directory :around
                (lambda (orig)
                  (if (and (projects-multi-project-view-p)
                           (projects-current-window-project))
                      (let ((dir (projects-dir (projects-current-window-project))))
                        (message "[projects] claude-code--directory override: %s -> %s"
                                 (projects-current-window-project) dir)
                        dir)
                    (funcall orig))))
    (add-hook 'vterm-mode-hook #'projects--find-file-hook)
    (add-hook 'eat-mode-hook   #'projects--find-file-hook)
    (add-hook 'dired-mode-hook #'projects--find-file-hook)
    (add-hook 'window-configuration-change-hook #'projects--maybe-close-info-window)
    ;; In multi-project mode: re-register buffers appearing in windows and keep headers fresh
    (add-hook 'window-buffer-change-functions #'projects--window-buffer-change-hook)
    ;; Track keyboard focus for header-line active/inactive rendering
    (add-hook 'post-command-hook #'projects--track-focused-window)
    ;; quit-window buries buffers (no kill-buffer-hook) — fix windows afterwards
    (advice-add 'quit-window :after (lambda (&rest _)
                                      (projects--fix-windows-after-kill)))
    ;; dirvish-quit kills dired buffers then calls quit-window, leaving windows on
    ;; scratch. Advise it to fix windows after the whole session is torn down.
    (with-eval-after-load 'dirvish
      (advice-add 'dirvish-quit :after (lambda (&rest _)
                                         (run-with-timer 0 nil #'projects--fix-windows-after-kill))))
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
          (projects-save))))))

(projects--setup-hooks)

;;; ---------------------------------------------------------------------------
;;; Display Rules
;;; ---------------------------------------------------------------------------

(defun projects--ordinary-window-count ()
  "Return number of non-minibuffer windows in the selected frame."
  (length (window-list nil 0)))

(defun projects--maybe-close-info-window ()
  "Close any project info buffer window when other windows are present.
Runs on `window-configuration-change-hook' so opening a special buffer
alongside the info buffer collapses to fullscreen.
Deferred via idle timer so window-size-change-functions fire naturally,
allowing vterm/eat to resize correctly."
  (if (projects-multi-project-view-p)
      (message "[projects] maybe-close-info: skipping (multi-project mode, win-count=%d)"
               (projects--ordinary-window-count))
  (when (> (projects--ordinary-window-count) 1)
    (message "[projects] maybe-close-info: checking (single-project mode, win-count=%d proj=%s)"
             (projects--ordinary-window-count) (projects-current))
    (let* ((frame (selected-frame))
           (info-win
            (or
             ;; Normal case: a window is showing a project info buffer
             (cl-find-if
              (lambda (w)
                (string-match-p "^\\*project: "
                                (buffer-name (window-buffer w))))
              (window-list nil 0))
             ;; Fresh client frame: a terminal buffer (vterm/eat/claude) was
             ;; opened alongside the project file/info buffer. The new buffer's
             ;; window is always selected — treat the NON-selected window as the
             ;; one to replace. Guard against transient/menu windows firing this.
             (when (frame-parameter frame 'projects-fresh-client)
               (let* ((sel (selected-window))
                      (sel-name (buffer-name (window-buffer sel))))
                 (when (or (with-current-buffer (window-buffer sel)
                             (derived-mode-p 'vterm-mode 'eat-mode))
                           (string-match-p "^\\*claude" sel-name))
                   ;; Also exclude side/dedicated windows as the "replacement" target
                   (cl-find-if (lambda (w)
                                 (and (not (eq w sel))
                                      (not (window-parameter w 'window-side))
                                      (not (window-dedicated-p w))))
                               (window-list nil 0))))))))
      (when info-win
        (set-frame-parameter frame 'projects-fresh-client nil)
        (run-with-idle-timer
         0 nil
         (lambda (info-win)
           (when (and (window-live-p info-win)
                      (> (projects--ordinary-window-count) 1))
             ;; Find a non-info sibling window to steal its buffer
             (let ((other-win (cl-find-if
                               (lambda (w) (not (eq w info-win)))
                               (window-list nil 0))))
               (when (and other-win
                          ;; Skip side/popup windows (transient menus, which-key, etc.)
                          (not (window-parameter other-win 'window-side))
                          (not (window-dedicated-p other-win)))
                 (let ((buf (window-buffer other-win)))
                   ;; Show the other buffer in info-win, then delete other-win
                   (set-window-buffer info-win buf)
                   (delete-window other-win)
                   ;; Capture dimensions NOW — reading inside the timer risks
                   ;; getting wrong values if a split appears during the delay.
                   (let ((h (window-body-height info-win))
                         (w (window-max-chars-per-line info-win)))
                     (my/vterm-resize-log "close-info-window: split collapsed, captured h=%d w=%d for %s" h w (buffer-name buf))
                     ;; 2s delay allows Claude's TUI to finish initializing.
                     (run-with-timer
                      2 nil
                      (lambda ()
                        (when (buffer-live-p buf)
                          (with-current-buffer buf
                            (cond
                             ((derived-mode-p 'vterm-mode)
                              (my/vterm-resize-log "close-info-window 2s timer: calling size-refresh h=%d w=%d for %s" h w (buffer-name))
                              (when (get-buffer-process buf)
                                (my/vterm-size-refresh h w)))
                             ((and (eq major-mode 'eat-mode)
                                   (fboundp 'eat-reset))
                              (eat-reset)))))))))))))
         info-win))))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'projects)
;;; projects.el ends here
