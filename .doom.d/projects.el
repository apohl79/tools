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

;;; ---------------------------------------------------------------------------
;;; CRUD
;;; ---------------------------------------------------------------------------

(defun projects-create (name dir)
  "Create a new project named NAME with root directory DIR.
NAME must be unique. DIR is created if it does not exist."
  (interactive
   (let* ((name (read-string "Project name: "))
          (dir (read-directory-name "Project directory: " nil nil nil)))
     (list name (expand-file-name dir))))
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
    (message "Project '%s' deleted" name)))

;;; ---------------------------------------------------------------------------
;;; Switching
;;; ---------------------------------------------------------------------------

(defvar projects-switch-hook nil
  "Hook run after switching to a project.")

(defun projects-switch (name &optional norecord)
  "Switch to project NAME. Updates tab-bar and active buffers."
  (interactive
   (let ((candidates (cl-remove projects--current (projects-names-mru) :test #'equal)))
     (list (completing-read "Switch to project: "
                            (lambda (str pred action)
                              (if (eq action 'metadata)
                                  '(metadata (display-sort-function . identity))
                                (complete-with-action action candidates str pred)))
                            nil t))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (setq projects--current name)
  (unless norecord
    (let ((proj (gethash name projects--table)))
      (plist-put proj :switch-time (float-time))
      (puthash name proj projects--table)))
  ;; Set default directory to the project root so find-file etc. start there
  (when-let ((dir (projects-dir name)))
    (setq-default default-directory dir))
  ;; Show the project's buffers or the info buffer
  (projects--ensure-visible-buffer)
  (projects--tab-bar-refresh)
  (run-hooks 'projects-switch-hook))

;;; ---------------------------------------------------------------------------
;;; Buffer Management
;;; ---------------------------------------------------------------------------

(defun projects-register-buffer (buf &optional project-name)
  "Register BUF as belonging to PROJECT-NAME (defaults to current project).
Does nothing if BUF is a special/global buffer or if BUF is dead."
  (when (buffer-live-p buf)
    (let ((proj (or project-name projects--current)))
      (when (and proj (not (projects-special-buffer-p buf)))
        (with-current-buffer buf
          (setq-local projects--buffer-project proj))
        (let* ((entry (gethash proj projects--table))
               (bufs (plist-get entry :buffers)))
          (unless (memq buf bufs)
            (plist-put entry :buffers (cons buf bufs))
            (puthash proj entry projects--table)))))))

(defun projects--find-file-hook ()
  "Register newly opened files with the current project."
  (projects-register-buffer (current-buffer)))

(defun projects--cleanup-dead-buffers ()
  "Remove the current (dying) buffer from its owning project's buffer list."
  (let ((proj projects--buffer-project)
        (buf (current-buffer)))
    (when proj
      (let* ((entry (gethash proj projects--table))
             (bufs (plist-get entry :buffers)))
        (when (memq buf bufs)
          (plist-put entry :buffers (delq buf bufs))
          (puthash proj entry projects--table))))))

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
        (insert (propertize "  This project has no open buffers.\n"
                            'face 'font-lock-doc-face))
        (insert "  Open a file with C-x C-f to get started.\n"))
      (setq buffer-read-only t)
      (goto-char (point-min)))
    buf))

(defun projects-show-info ()
  "Show the info buffer for the current project."
  (interactive)
  (if projects--current
      (switch-to-buffer (projects--create-info-buffer projects--current))
    (user-error "No active project")))

(defun projects--ensure-visible-buffer ()
  "Ensure visible windows show buffers appropriate for the current project.
Replaces any windows showing a foreign project's info buffer."
  (let* ((proj projects--current)
         (bufs (when proj (projects-buffers proj)))
         ;; Filter to non-info, non-special file buffers
         (file-bufs (when bufs
                      (cl-remove-if
                       (lambda (b)
                         (or (projects-special-buffer-p b)
                             (string-match-p "^\\*project: " (buffer-name b))))
                       bufs))))
    (when proj
      ;; Replace any windows showing another project's info buffer
      (dolist (win (window-list))
        (let ((bname (buffer-name (window-buffer win))))
          (when (and (string-match-p "^\\*project: " bname)
                     (not (string= bname (projects--info-buffer-name proj))))
            (with-selected-window win
              (switch-to-buffer (projects--create-info-buffer proj))))))
      ;; Handle the selected window
      (if (null file-bufs)
          (switch-to-buffer (projects--create-info-buffer proj))
        (let ((visible (cl-find-if
                        (lambda (b)
                          (with-current-buffer b
                            (equal projects--buffer-project proj)))
                        (buffer-list))))
          (when visible
            (switch-to-buffer visible)))))))

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

(defun projects--tab-bar-format ()
  "Tab-bar format function: renders all projects MRU-sorted with active one highlighted.
Reuses faces my/workspace-tab-active and my/workspace-tab-inactive from +functions.el."
  (let* ((current projects--current)
         (mru (projects-names-mru))
         ;; Active project always first, remaining projects MRU-sorted
         (names (if current
                    (cons current (cl-remove current mru :test #'equal))
                  mru)))
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

          ;; Restore active project
          (if (and saved-current (gethash saved-current projects--table))
              (setq projects--current saved-current)
            (setq projects--current (car (projects-names-mru))))

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
                              (+doom-dashboard-reload t)))))))))

;;; ---------------------------------------------------------------------------
;;; Setup Hooks
;;; ---------------------------------------------------------------------------

(defvar projects--hooks-installed-p nil
  "Non-nil if projects hooks have already been installed.")

(defun projects--setup-hooks ()
  "Install all hooks for buffer tracking, auto-save, and frame management.
Idempotent: safe to call multiple times."
  (unless projects--hooks-installed-p
    (setq projects--hooks-installed-p t)
    (add-hook 'find-file-hook #'projects--find-file-hook)
    (add-hook 'kill-buffer-hook #'projects--cleanup-dead-buffers)
    ;; Register terminal buffers with the current project on creation
    (add-hook 'vterm-mode-hook #'projects--find-file-hook)
    (add-hook 'eat-mode-hook   #'projects--find-file-hook)
    (add-hook 'window-configuration-change-hook #'projects--maybe-close-info-window)
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
  (when (> (projects--ordinary-window-count) 1)
    (let ((info-win (cl-find-if
                     (lambda (w)
                       (string-match-p "^\\*project: "
                                       (buffer-name (window-buffer w))))
                     (window-list nil 0))))
      (when info-win
        (run-with-idle-timer
         0 nil
         (lambda (info-win)
           (when (and (window-live-p info-win)
                      (> (projects--ordinary-window-count) 1))
             ;; Find a non-info sibling window to steal its buffer
             (let ((other-win (cl-find-if
                               (lambda (w) (not (eq w info-win)))
                               (window-list nil 0))))
               (when other-win
                 (let ((buf (window-buffer other-win)))
                   ;; Show the other buffer in info-win, then delete other-win
                   (set-window-buffer info-win buf)
                   (delete-window other-win)
                   ;; Force full redraw via resize-bounce: shrink by 1 col then
                   ;; restore. Two distinct SIGWINCHs guarantee any TUI redraws.
                   ;; 2s delay allows Claude's TUI to finish initializing first.
                   (run-with-timer
                    2 nil
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (cond
                           ((derived-mode-p 'vterm-mode)
                            (let* ((win  (get-buffer-window buf))
                                   (proc (get-buffer-process buf)))
                              (when (and win proc (process-live-p proc))
                                (let ((h (window-body-height win))
                                      (w (window-max-chars-per-line win)))
                                  (set-process-window-size proc h (max 1 (1- w)))
                                  (run-with-timer
                                   0.05 nil
                                   (lambda ()
                                     (when (process-live-p proc)
                                       (set-process-window-size proc h w))))))))
                           ((and (eq major-mode 'eat-mode)
                                 (fboundp 'eat-reset))
                            (eat-reset))))))))))))
         info-win)))))

;;; ---------------------------------------------------------------------------
;;; Provide
;;; ---------------------------------------------------------------------------

(provide 'projects)
;;; projects.el ends here
