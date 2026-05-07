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

(defconst projects-cmux--trusted-prefixes
  '("/opt/homebrew/bin" "/usr/local/bin" "/bin" "/usr/bin")
  "Trusted directory prefixes for the resolved cmux executable.")

(defun projects-cmux--resolve-cmux ()
  "Resolve the cmux binary via `executable-find' at load time.
Return the absolute path if found, otherwise the literal string
\"cmux\" (so PATH lookup still happens at call time as a fallback).
Logs an informational message when the resolved path is outside the
known trusted prefixes."
  (let ((resolved (executable-find "cmux")))
    (cond
     ((null resolved)
      (message "[projects-cmux] cmux not found on PATH at load time")
      "cmux")
     ((cl-some (lambda (prefix)
                 (string-prefix-p (file-name-as-directory prefix) resolved))
               projects-cmux--trusted-prefixes)
      resolved)
     (t
      (message "[projects-cmux] using cmux at %s" resolved)
      resolved))))

(defvar projects-cmux--cmux-command (projects-cmux--resolve-cmux)
  "Path to the cmux CLI. Resolved at load time. Overridable for tests.")

(defun projects-cmux--call (&rest args)
  "Run cmux with ARGS. Return exit code; log stderr/stdout to *projects-cmux*."
  (let ((buf (get-buffer-create "*projects-cmux*")))
    (with-current-buffer buf (goto-char (point-max)))
    (condition-case err
        (let ((exit (apply #'call-process projects-cmux--cmux-command
                           nil (list buf t) nil args)))
          (unless (zerop exit)
            (message "[projects-cmux] cmux %s → exit %d (see *projects-cmux*)"
                     (mapconcat #'identity args " ") exit))
          exit)
      (error
       (with-current-buffer buf
         (goto-char (point-max))
         (insert (format "[projects-cmux] error invoking cmux %s: %s\n"
                         (mapconcat #'identity args " ")
                         (error-message-string err))))
       (message "[projects-cmux] error invoking cmux %s: %s"
                (mapconcat #'identity args " ")
                (error-message-string err))
       127))))

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

(defconst projects-cmux--valid-name-regexp
  "\\`[A-Za-z0-9._ -]+\\'"
  "Regexp matching project names safe to embed in shell commands.")

(defconst projects-cmux--valid-daemon-regexp
  "\\`[A-Za-z0-9._-]+\\'"
  "Regexp matching daemon names safe to embed in shell commands.")

(defun projects-cmux--validate-name (name)
  "Reject NAME if it contains characters unsafe for shell embedding.
Signal `user-error' on rejection."
  (unless (and (stringp name)
               (string-match-p projects-cmux--valid-name-regexp name))
    (user-error "Invalid project name: %s" name)))

(defun projects-cmux--emacsclient-command (project)
  "Return the shell command used to attach an emacsclient frame for PROJECT.
The daemon name and the `-F' payload are shell-quoted so embedded shell
metacharacters in PROJECT cannot escape the surrounding quoting."
  (let* ((raw-daemon (getenv "EMACS_DAEMON_NAME"))
         (daemon (cond
                  ((null raw-daemon) "cmux")
                  ((string-match-p projects-cmux--valid-daemon-regexp raw-daemon)
                   raw-daemon)
                  (t
                   (projects--log
                    "EMACS_DAEMON_NAME %S failed validation; falling back to \"cmux\""
                    raw-daemon)
                   "cmux")))
         (frame-arg (format "((projects-project . %S))" project)))
    (format "emacsclient -s %s -t -F %s"
            (shell-quote-argument daemon)
            (shell-quote-argument frame-arg))))

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
  (projects-cmux--validate-name name)
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
  (projects-cmux--validate-name new-name)
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

;;; ---------------------------------------------------------------------------
;;; Layout
;;; ---------------------------------------------------------------------------

(defconst projects--multi-layouts '("1x1" "2x1" "2x2" "3x2"))

(defun projects-cmux--split (direction)
  "Split current pane in DIRECTION (\"left\"/\"right\"/\"up\"/\"down\")."
  (projects-cmux--call "new-split" direction))

(defun projects-cmux--new-split-with-ref (direction)
  "Run `cmux new-split DIRECTION' and return the new surface ref.
The ref is the first whitespace-delimited token of stdout's first line,
or nil if cmux failed or produced no output."
  (with-temp-buffer
    (let ((exit (condition-case _
                    (call-process projects-cmux--cmux-command nil
                                  (current-buffer) nil
                                  "new-split" direction)
                  (error 127))))
      (when (and (integerp exit) (zerop exit))
        (goto-char (point-min))
        (let ((line (buffer-substring-no-properties
                     (point-min) (line-end-position))))
          (car (split-string line "[ \t]+" t)))))))

(defun projects-cmux--current-pane-ref ()
  "Return the current cmux pane reference, or nil on failure.
Parses the first whitespace-delimited token of `cmux current-workspace'."
  (with-temp-buffer
    (let ((exit (condition-case _
                    (call-process projects-cmux--cmux-command nil
                                  (current-buffer) nil
                                  "current-workspace")
                  (error 127))))
      (when (and (integerp exit) (zerop exit))
        (goto-char (point-min))
        (let ((line (buffer-substring-no-properties
                     (point-min) (line-end-position))))
          (car (split-string line "[ \t]+" t)))))))

(defun projects-cmux--focus-pane (ref)
  "Focus cmux pane REF if non-nil."
  (when ref (projects-cmux--call "focus-pane" "--pane" ref)))

(defun projects-cmux--current-workspace-project ()
  "Return the workspace project name reported by `cmux current-workspace'.
Parses output of the form `<ref>\\tname=<name>\\n' (case-insensitive,
tab-separated key=value pairs after the ref). On parse failure or cmux
error, falls back to `(projects-current)' and logs a warning."
  (let* ((buf (get-buffer-create " *projects-cmux-ws*"))
         (exit (with-current-buffer buf
                 (erase-buffer)
                 (condition-case err
                     (call-process projects-cmux--cmux-command
                                   nil (current-buffer) nil
                                   "current-workspace")
                   (error
                    (projects--log "current-workspace error: %s"
                                   (error-message-string err))
                    127))))
         (output (with-current-buffer buf (buffer-string))))
    (or (and (integerp exit) (zerop exit)
             (let ((case-fold-search t)
                   (line (car (split-string output "\n" t))))
               (when line
                 (let* ((fields (split-string line "\t" t))
                        (kvs (cdr fields))
                        (name nil))
                   (dolist (kv kvs)
                     (when (string-match "\\`\\([^=]+\\)=\\(.*\\)\\'" kv)
                       (when (string-equal (downcase (match-string 1 kv)) "name")
                         (setq name (match-string 2 kv)))))
                   name))))
        (progn
          (projects--log "current-workspace: parse failed, falling back to projects-current")
          (projects-current)))))

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
  (let ((proj (projects-cmux--current-workspace-project)))
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
      ("3x2"
       ;; Build three columns by splitting right twice, then split each
       ;; column down once for two rows. Five new panes total.
       ;; Capture each column's pane ref so the second-row splits target
       ;; the correct column instead of cascading on the most-recent pane.
       (let ((c1 (projects-cmux--current-pane-ref))
             c2 c3)
         (setq c2 (projects-cmux--new-split-with-ref "right"))
         (projects-cmux--send-emacsclient proj)
         (setq c3 (projects-cmux--new-split-with-ref "right"))
         (projects-cmux--send-emacsclient proj)
         (when c1
           (projects-cmux--focus-pane c1)
           (projects-cmux--split "down")
           (projects-cmux--send-emacsclient proj))
         (when c2
           (projects-cmux--focus-pane c2)
           (projects-cmux--split "down")
           (projects-cmux--send-emacsclient proj))
         (when c3
           (projects-cmux--focus-pane c3)
           (projects-cmux--split "down")
           (projects-cmux--send-emacsclient proj))))
      (_ (user-error "Unknown layout: %s" layout)))))

;;; ---------------------------------------------------------------------------
;;; Browser
;;; ---------------------------------------------------------------------------

(defun projects-cmux--browse-url (url &rest _)
  "Open URL in a new cmux browser pane in the current workspace.
Only http://, https://, and mailto: URLs are allowed; all other
schemes are refused with a warning to avoid passing
javascript:/file://data:/custom-scheme URLs to cmux."
  (if (and (stringp url)
           (string-match-p "\\`\\(?:https?://\\|mailto:\\)" url))
      (projects-cmux--call "new-pane" "--type" "browser" "--url" url)
    (message "[projects-cmux] refusing browse-url: %s (unsupported scheme)" url)
    nil))

(setq browse-url-browser-function #'projects-cmux--browse-url)

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

;;; ---------------------------------------------------------------------------
;;; Git clone (ported from projects.el)
;;; ---------------------------------------------------------------------------

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
    (projects--log "cloning %s into %s..." repo-url target)
    (let ((buf (get-buffer-create "*projects-clone*")))
      (with-current-buffer buf (erase-buffer))
      (let ((exit-code (apply #'call-process (car cmd) nil (list buf t) nil (cdr cmd))))
        (if (eq exit-code 0)
            (progn
              (projects--log "clone succeeded")
              (unless (file-directory-p target)
                (user-error "Clone reported success but target directory '%s' was not created — see *projects-clone* buffer" target))
              (projects-create project-name target))
          (user-error "Clone failed (exit %s) — see *projects-clone* buffer"
                      exit-code))))))

;;; ---------------------------------------------------------------------------
;;; Public-command surface stubs
;;;
;;; These names are referenced from `config.el', `+functions.el', and the
;;; tab-bar/ibuffer setup. The wezterm-flavored `projects.el' implements them
;;; in full. In cmux mode we provide no-op stubs so `void-function' errors
;;; never reach the user; the cmux backend is intentionally minimal.
;;; ---------------------------------------------------------------------------

(defun projects-restore ()
  "Cmux-mode no-op stub for `projects-restore'.
Called from a startup `run-with-timer'; must never error."
  (interactive)
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-restore)
  nil)

(defun projects-save ()
  "Cmux-mode no-op stub for `projects-save'."
  (interactive)
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-save)
  nil)

(defun projects-show-info ()
  "Cmux-mode no-op stub for `projects-show-info'."
  (interactive)
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-show-info)
  nil)

(defun projects-switch-buffer (&optional _arg)
  "Cmux-mode no-op stub for `projects-switch-buffer'.
Accepts the optional prefix arg of the wezterm version for signature parity."
  (interactive "P")
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-switch-buffer)
  nil)

(defun projects-switch-dispatch ()
  "Cmux-mode no-op stub for `projects-switch-dispatch'."
  (interactive)
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-switch-dispatch)
  nil)

(defun projects-move-buffer ()
  "Cmux-mode no-op stub for `projects-move-buffer'."
  (interactive)
  (message "[projects-cmux] %s: not implemented in cmux mode" 'projects-move-buffer)
  nil)

(defun projects--create-info-buffer (project)
  "Cmux-mode stub for `projects--create-info-buffer'.
Returns a temp buffer named *project: PROJECT* with a single info line."
  (let ((buf (get-buffer-create (format "*project: %s*" project))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "[projects-cmux] info buffer for project %s: not implemented in cmux mode\n"
                        project))))
    buf))

(defun projects--tab-bar-format ()
  "Cmux-mode stub for `projects--tab-bar-format'.
Returns nil so `tab-bar-format' shows nothing extra."
  nil)

(defun projects--ibuffer-setup ()
  "Cmux-mode no-op stub for `projects--ibuffer-setup'."
  nil)

(defun projects-register-buffer (_buffer &optional _project-name)
  "Cmux-mode no-op stub for `projects-register-buffer'."
  nil)

(defun projects-multi-project-view-p ()
  "Cmux-mode stub: always nil — no multi-project view in cmux mode."
  nil)

(defun projects-current-window-project ()
  "Cmux-mode stub: return the global current project.
Keeps `+functions.el' callers functional without window-tracking logic."
  (projects-current))

(provide 'projects-cmux)
;;; projects-cmux.el ends here
