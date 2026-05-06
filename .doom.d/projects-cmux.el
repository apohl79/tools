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
;;; Accessors (pure — copied from projects.el)
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

(provide 'projects-cmux)
;;; projects-cmux.el ends here
