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

(defun projects-cmux--frame-show-info (project)
  "Switch FRAME's selected window to PROJECT's info buffer."
  (let ((buf (projects--create-info-buffer project)))
    (when (buffer-live-p buf)
      (switch-to-buffer buf))))

(defun projects-cmux--frame-init (frame)
  "Bind FRAME to a project: prefer the explicit `projects-project' frame
parameter, else look up a project whose `:dir' contains the frame's
`cmux-cwd' parameter (the cwd from which `tools/emacs' was invoked).
On match, switch the frame's project AND open the project info buffer."
  (let* ((explicit (frame-parameter frame 'projects-project))
         (cwd (frame-parameter frame 'cmux-cwd))
         (proj (or (and explicit (gethash explicit projects--table) explicit)
                   (and cwd (projects--find-project-for-file cwd)))))
    (when proj
      (with-selected-frame frame
        (projects-switch proj)
        (projects-cmux--frame-show-info proj)))))

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
  "Create project NAME at DIR (Emacs-only — does not touch cmux).
The user creates cmux workspaces manually; the wrapper's `tools/emacs'
finds the matching project by cwd when invoked inside one."
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
    name))

(defun projects-delete (name)
  "Delete project NAME (Emacs-only — does not touch cmux)."
  (interactive
   (list (completing-read "Delete project: " (projects-names) nil t
                          nil nil (projects-current))))
  (unless (gethash name projects--table)
    (user-error "Project '%s' does not exist" name))
  (when (yes-or-no-p (format "Delete project '%s' and kill its buffers? " name))
    (dolist (buf (projects-buffers name))
      (kill-buffer buf))
    (remhash name projects--table)
    (when (equal projects--current name)
      (setq projects--current (car (projects-names-visible))))
    (projects--log "delete: %s" name)))

(defun projects-rename (old-name new-name)
  "Rename project OLD-NAME to NEW-NAME (Emacs-only — does not touch cmux)."
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

(defun projects-cmux--list-workspaces ()
  "Parse `cmux list-workspaces' into a list of (NAME . REF) cons cells.
NAME is the trimmed workspace name; REF is the `workspace:N' identifier.
A given NAME may appear multiple times if cmux has duplicates."
  (with-temp-buffer
    (let ((rc (call-process projects-cmux--cmux-command nil (current-buffer) nil
                            "list-workspaces"))
          result)
      (when (zerop rc)
        (goto-char (point-min))
        (while (not (eobp))
          ;; Strip optional leading "* " (selected marker).
          (when (looking-at "\\s-*\\*?\\s-*\\(workspace:[0-9]+\\)\\s-+\\(.*\\)\\s-*$")
            (let ((ref (match-string 1))
                  (rest (string-trim (match-string 2))))
              ;; Drop any " [foo]" trailing annotations the user likely doesn't
              ;; want as part of the name (e.g. "[selected]", "[color]").
              (when (string-match "\\`\\(.*?\\)\\(\\s-+\\[[^]]*\\]\\)*\\s-*\\'" rest)
                (setq rest (string-trim (match-string 1 rest))))
              (push (cons rest ref) result)))
          (forward-line 1)))
      (nreverse result))))

(defun projects-cmux--workspace-names ()
  "Set of cmux workspace names (as a hash-table)."
  (let ((set (make-hash-table :test #'equal)))
    (dolist (entry (projects-cmux--list-workspaces))
      (puthash (car entry) t set))
    set))

(defun projects-cmux-resync (&optional lazy)
  "Sync every project to a cmux workspace with an attached emacsclient frame.

For each project:
- if a cmux workspace with that name already exists, send the
  emacsclient command to its shell (existing workspace stays put);
- else create a new workspace with `--command \"emacsclient ...\"'.

Idempotent: re-running does NOT create duplicate workspaces.

With LAZY prefix, only create empty workspaces (no emacsclient
command) — useful when the user prefers to attach frames manually.

Reports counts: NEW, ATTACHED (existing workspace; emacsclient
sent), SKIPPED (existing and lazy), FAILED. Returns NEW + ATTACHED."
  (interactive "P")
  (let ((existing (projects-cmux--workspace-names))
        (new 0) (attached 0) (skipped 0) (failed 0))
    (dolist (name (projects-names))
      (let* ((dir (projects-dir name))
             (cmd (unless lazy (projects-cmux--emacsclient-command name))))
        (cond
         ;; Already in cmux.
         ((gethash name existing)
          (cond
           ((null cmd) (cl-incf skipped))
           ((zerop (projects-cmux--call "send" "--workspace" name
                                        (concat cmd "\n")))
            (cl-incf attached))
           (t (cl-incf failed))))
         ;; Missing — create.
         (t
          (let* ((args `("new-workspace" "--name" ,name "--cwd" ,(or dir "")
                         ,@(when cmd (list "--command" cmd))))
                 (rc (apply #'projects-cmux--call args)))
            (if (zerop rc) (cl-incf new) (cl-incf failed)))))))
    (message "[projects-cmux] resync: %d new, %d attached, %d skipped, %d failed%s%s"
             new attached skipped failed
             (if lazy " (lazy: no emacsclient)" "")
             (if (> failed 0) " — see *projects-cmux*" ""))
    (+ new attached)))

(defun projects-cmux-cleanup-duplicates (&optional dry-run)
  "Close cmux workspaces whose name appears more than once.
For each duplicate-name group, keep the FIRST occurrence reported by
`cmux list-workspaces' and close the rest by ref. With DRY-RUN prefix,
print what would be closed without acting."
  (interactive "P")
  (let ((seen (make-hash-table :test #'equal))
        (closed 0) (kept 0) (failed 0)
        (dups nil))
    (dolist (entry (projects-cmux--list-workspaces))
      (let ((name (car entry))
            (ref (cdr entry)))
        (cond
         ((gethash name seen)
          (push (cons name ref) dups))
         (t
          (puthash name t seen)
          (cl-incf kept)))))
    (cond
     (dry-run
      (message "[projects-cmux] would close %d duplicate workspaces (keeping %d unique): %s"
               (length dups) kept
               (mapconcat (lambda (e) (format "%s/%s" (car e) (cdr e)))
                          dups ", ")))
     (t
      (dolist (e dups)
        (if (zerop (projects-cmux--call "close-workspace" "--workspace" (cdr e)))
            (cl-incf closed)
          (cl-incf failed)))
      (message "[projects-cmux] cleanup: closed %d, kept %d unique, %d failed%s"
               closed kept failed
               (if (> failed 0) " — see *projects-cmux*" ""))))
    (length dups)))

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

(defun projects-restore (&optional file)
  "Restore project metadata from FILE (default `projects--save-file').
Reads the same on-disk format produced by the wezterm `projects-save'
\(plist `(:version 1|2 :current NAME :projects ((NAME :dir :files :switch-time)…))').
Repopulates `projects--table' and sets `projects--current'. Does NOT
auto-open files (cmux owns layout); does NOT call cmux. Use
`projects-cmux-resync' afterwards to mirror to cmux workspaces."
  (interactive)
  (let ((file (or file projects--save-file)))
    (cond
     ((not (file-exists-p file))
      (message "[projects-cmux] no saved session at %s" file)
      nil)
     (t
      (let* ((data (with-temp-buffer
                     (insert-file-contents file)
                     (read (current-buffer))))
             (version (plist-get data :version))
             (saved-current (plist-get data :current))
             (project-list (plist-get data :projects)))
        (cond
         ((not (member version '(1 2)))
          (message "[projects-cmux] unknown session version: %s" version)
          nil)
         (t
          (clrhash projects--table)
          (dolist (entry project-list)
            (let ((name (car entry))
                  (pdata (cdr entry)))
              (puthash name
                       (list :dir (plist-get pdata :dir)
                             :buffers nil
                             :files (plist-get pdata :files)
                             :switch-time (or (plist-get pdata :switch-time) 0))
                       projects--table)))
          (setq projects--current
                (or (and saved-current (gethash saved-current projects--table)
                         saved-current)
                    (car (projects-names-mru))))
          (projects--log "restore: %d projects (current=%s)"
                         (hash-table-count projects--table)
                         projects--current)
          (hash-table-count projects--table))))))))

(defun projects-save ()
  "Persist `projects--table' to `projects--save-file' in version-2 format.
Same shape as the wezterm `projects-save', but without backup rotation
\(cmux mode is initial; revisit if backups become valuable)."
  (interactive)
  (make-directory (file-name-directory projects--save-file) t)
  (let ((entries (mapcar (lambda (name)
                           (let ((p (gethash name projects--table)))
                             (cons name
                                   (list :dir (plist-get p :dir)
                                         :files (plist-get p :files)
                                         :switch-time (plist-get p :switch-time)))))
                         (projects-names))))
    (with-temp-file projects--save-file
      (let ((print-length nil)
            (print-level nil))
        (pp (list :version 2
                  :current projects--current
                  :projects entries)
            (current-buffer)))))
  (message "[projects-cmux] saved %d projects to %s"
           (hash-table-count projects--table) projects--save-file))

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

(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'projects-info-mode 'emacs))
(with-eval-after-load 'evil
  (when (fboundp 'evil-set-initial-state)
    (evil-set-initial-state 'projects-info-mode 'emacs)))

(defun projects-show-info ()
  "Show the info buffer for the current frame's project."
  (interactive)
  (let ((proj (projects-current)))
    (if proj
        (switch-to-buffer (projects--create-info-buffer proj))
      (user-error "No active project"))))

(defun projects-switch-buffer ()
  "Switch to a buffer belonging to the current frame's project.
Falls back to special/global buffers."
  (interactive)
  (let* ((proj (projects-current))
         (project-bufs (when proj
                         (cl-remove-if-not
                          #'buffer-live-p
                          (plist-get (gethash proj projects--table) :buffers))))
         (special-bufs (cl-remove-if-not
                        (lambda (b)
                          (and (projects-special-buffer-p b)
                               (not (string-prefix-p " " (buffer-name b)))))
                        (buffer-list)))
         (ordered (append project-bufs special-bufs))
         (names (mapcar #'buffer-name ordered)))
    (switch-to-buffer
     (completing-read (format "Buffer [%s]: " (or proj "global"))
                      names nil t))))

(defun projects-switch-dispatch ()
  "Switch the current frame to another project (interactive)."
  (interactive)
  (call-interactively #'projects-switch))

(defun projects-move-buffer (buffer target-project)
  "Move BUFFER to TARGET-PROJECT."
  (interactive
   (list (current-buffer)
         (completing-read "Move buffer to project: " (projects-names) nil t)))
  (when (projects-special-buffer-p buffer)
    (user-error "Cannot move a global/special buffer to a project"))
  (let ((old-proj (with-current-buffer buffer projects--buffer-project)))
    (when old-proj
      (let* ((entry (gethash old-proj projects--table))
             (bufs (plist-get entry :buffers)))
        (plist-put entry :buffers (delq buffer bufs))
        (puthash old-proj entry projects--table)))
    (projects-register-buffer buffer target-project)
    (message "Buffer '%s' moved to project '%s'"
             (buffer-name buffer) target-project)))

(defun projects--info-buffer-name (project-name)
  "Return the info buffer name for PROJECT-NAME."
  (format "*project: %s*" project-name))

(defun projects--create-info-buffer (project-name)
  "Create or refresh the read-only info buffer for PROJECT-NAME.
Returns the buffer."
  (let* ((buf-name (projects--info-buffer-name project-name))
         (buf (get-buffer-create buf-name))
         (entry (gethash project-name projects--table))
         (dir (plist-get entry :dir)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local projects--buffer-project nil)
        (setq-local default-directory (or dir "~/"))
        (insert "\n\n")
        (insert (propertize "  Project: " 'face '(:weight bold :height 1.4)))
        (let ((blue (and (fboundp 'doom-color) (doom-color 'blue))))
          (insert (propertize (format "%s\n" project-name)
                              'face `(:weight bold :height 1.4
                                      ,@(when blue (list :foreground blue))))))
        (insert (propertize (format "  Directory: %s\n"
                                    (abbreviate-file-name (or dir "~/")))
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

(defun projects--tab-bar-format ()
  "Cmux-mode stub for `projects--tab-bar-format'.
Returns nil so `tab-bar-format' shows nothing extra."
  nil)

(defun projects-ibuffer-groups ()
  "Return ibuffer filter groups for current projects, with 'Other' fallback."
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
  "Set up ibuffer to use project filter groups."
  (when (boundp 'ibuffer-filter-groups)
    (setq ibuffer-filter-groups (projects-ibuffer-groups))
    (when (fboundp 'ibuffer-update)
      (ibuffer-update nil t))))

(defun projects--find-project-for-file (file)
  "Return the project whose directory contains FILE, longest-prefix wins."
  (when file
    (let ((best-name nil)
          (best-len 0))
      (maphash (lambda (name entry)
                 (let ((dir (plist-get entry :dir)))
                   (when (and dir (string-prefix-p (expand-file-name dir)
                                                   (expand-file-name file)))
                     (let ((len (length dir)))
                       (when (> len best-len)
                         (setq best-name name best-len len))))))
               projects--table)
      best-name)))

(defun projects-register-buffer (buf &optional project-name)
  "Tag BUF as belonging to PROJECT-NAME (or current frame's project).
Validates the buffer's path against the project's directory; on mismatch
re-routes to the project that actually owns the path. Skips special
buffers and buffers already tagged."
  (when (buffer-live-p buf)
    (let ((existing (buffer-local-value 'projects--buffer-project buf)))
      (cond
       (existing
        (projects--log "register-buffer: SKIP buf=%s existing-proj=%s requested=%s"
                       (buffer-name buf) existing
                       (or project-name (projects-current))))
       ((projects-special-buffer-p buf)
        (projects--log "register-buffer: SKIP buf=%s (special/global)"
                       (buffer-name buf)))
       (t
        (let* ((requested (or project-name (projects-current)))
               (path (or (buffer-file-name buf)
                         (with-current-buffer buf
                           (expand-file-name default-directory))))
               (path-corrected nil)
               (proj (if (and path requested)
                         (let ((dir (projects-dir requested)))
                           (if (and dir (string-prefix-p (expand-file-name dir)
                                                         path))
                               requested
                             (let ((found (projects--find-project-for-file path)))
                               (when (and found (not (equal found requested)))
                                 (setq path-corrected found))
                               (or found requested))))
                       requested)))
          (when proj
            (projects--log "register-buffer: buf=%s proj=%s%s path=%s"
                           (buffer-name buf) proj
                           (if path-corrected
                               (format " (path-corrected from %s)" requested)
                             "")
                           (or path "<none>"))
            (with-current-buffer buf
              (setq-local projects--buffer-project proj))
            (let* ((entry (gethash proj projects--table))
                   (bufs (plist-get entry :buffers)))
              (unless (memq buf bufs)
                (plist-put entry :buffers (cons buf bufs))
                (puthash proj entry projects--table))))))))))

(defun projects--cleanup-dead-buffers ()
  "kill-buffer-hook: drop the dying buffer from its project's :buffers list."
  (let ((proj projects--buffer-project)
        (buf (current-buffer)))
    (when proj
      (let* ((entry (gethash proj projects--table))
             (bufs (plist-get entry :buffers)))
        (when (memq buf bufs)
          (projects--log "buffer-killed: %s from project %s" (buffer-name buf) proj)
          (plist-put entry :buffers (delq buf bufs))
          (puthash proj entry projects--table))))))

(defun projects--find-file-hook ()
  "find-file-hook: register the new buffer with the current frame's project."
  (let ((buf (current-buffer))
        (proj (projects-current)))
    (when proj
      (projects-register-buffer buf proj))))

(defvar projects-cmux--hooks-installed-p nil
  "Idempotency guard for hook installation on file reload.")

(unless projects-cmux--hooks-installed-p
  (setq projects-cmux--hooks-installed-p t)
  (add-hook 'find-file-hook #'projects--find-file-hook)
  (add-hook 'kill-buffer-hook #'projects--cleanup-dead-buffers)
  (with-eval-after-load 'vterm
    (add-hook 'vterm-mode-hook #'projects--find-file-hook))
  (with-eval-after-load 'eat
    (add-hook 'eat-mode-hook #'projects--find-file-hook))
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook #'projects--find-file-hook)))

(defun projects-multi-project-view-p ()
  "Cmux-mode stub: always nil — no multi-project view in cmux mode."
  nil)

(defun projects-current-window-project ()
  "Cmux-mode stub: return the global current project.
Keeps `+functions.el' callers functional without window-tracking logic."
  (projects-current))

(provide 'projects-cmux)
;;; projects-cmux.el ends here
