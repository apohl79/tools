;;; test-projects-cmux.el --- ERT tests for projects-cmux  -*- lexical-binding: t; -*-
(require 'ert)

(defvar projects-cmux-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory containing this test file. Captured at load time because
`load-file-name' is nil during ERT test execution.")

(load (expand-file-name "projects-cmux.el" projects-cmux-test--dir) nil t)

(ert-deftest projects-cmux/loads-cleanly ()
  (should (boundp 'projects--table))
  (should (hash-table-p projects--table))
  (should (fboundp 'projects-current))
  (should (fboundp 'projects-names))
  (should (fboundp 'projects-cmux--call)))

(ert-deftest projects-cmux/frame-init-binds-frame ()
  "Frames created with projects-project parameter switch to that project."
  ;; NOTE: `make-frame' cannot create a tty frame inside `emacs --batch'
  ;; ("Unknown terminal type"). We substitute the existing batch frame and
  ;; set its `projects-project' parameter to exercise the same code path:
  ;; `projects-cmux--frame-init' reads the frame parameter, looks up the
  ;; project, and calls `projects-switch' under `with-selected-frame'.
  (puthash "alpha" (list :dir "/tmp/alpha/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let ((frame (selected-frame))
        (saved-param (frame-parameter (selected-frame) 'projects-project))
        (saved-current projects--current))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'projects-project "alpha")
          (projects-cmux--frame-init frame)
          (should (equal (frame-parameter frame 'projects-project) "alpha"))
          (should (equal projects--current "alpha")))
      (set-frame-parameter frame 'projects-project saved-param)
      (setq projects--current saved-current)
      (remhash "alpha" projects--table))))

(ert-deftest projects-cmux/create-records-without-cmux ()
  "projects-create updates the in-memory model and does NOT call cmux —
cmux workspaces are created manually by the user."
  (let* ((capture (make-temp-file "cmux-cap"))
         (beta-dir (file-name-as-directory (make-temp-file "cmux-beta-" t)))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir)))
    (unwind-protect
        (progn
          (projects-create "beta" beta-dir)
          (should (gethash "beta" projects--table))
          (should (equal (projects-dir "beta") beta-dir))
          ;; cmux must NOT have been called.
          (should (= 0 (nth 7 (file-attributes capture)))))
      (remhash "beta" projects--table)
      (delete-file capture)
      (when (file-directory-p beta-dir)
        (delete-directory beta-dir t)))))

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
                                       projects-cmux-test--dir)))
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
                                       projects-cmux-test--dir)))
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

(ert-deftest projects-cmux/delete-removes-without-cmux ()
  "projects-delete removes the project from the model and does NOT call cmux."
  (puthash "epsilon" (list :dir "/tmp/epsilon/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir)))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (projects-delete "epsilon"))
          (should-not (gethash "epsilon" projects--table))
          (should (= 0 (nth 7 (file-attributes capture)))))
      (remhash "epsilon" projects--table)
      (delete-file capture))))

(ert-deftest projects-cmux/rename-rewrites-frames-without-cmux ()
  "projects-rename rekeys the table and rewrites frame parameters; does NOT call cmux."
  (puthash "zeta" (list :dir "/tmp/zeta/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir))
         (frame (selected-frame))
         (saved-param (frame-parameter frame 'projects-project)))
    (unwind-protect
        (progn
          (set-frame-parameter frame 'projects-project "zeta")
          (projects-rename "zeta" "eta")
          (should (gethash "eta" projects--table))
          (should-not (gethash "zeta" projects--table))
          (should (equal (frame-parameter frame 'projects-project) "eta"))
          (should (= 0 (nth 7 (file-attributes capture)))))
      (set-frame-parameter frame 'projects-project saved-param)
      (remhash "zeta" projects--table)
      (remhash "eta" projects--table)
      (delete-file capture))))

(ert-deftest projects-cmux/set-layout-2x2-orchestrates-splits ()
  (puthash "theta" (list :dir "/tmp/theta/" :buffers nil :files nil :switch-time 0)
           projects--table)
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir))
         (projects--current "theta"))
    (unwind-protect
        (progn
          (projects-set-layout "2x2")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              ;; Three splits should have been created.
              ;; Note: the plan's `(cl-count ?\\n (split-string s "new-split" t))'
              ;; always yields 0 (cl-count of a character against a list of
              ;; strings), making the assertion `(>= 0 3)' permanently false.
              ;; Substituted with a direct count of "new-split" occurrences in
              ;; the capture, which faithfully encodes the comment's intent.
              (let ((splits 0)
                    (start 0))
                (while (string-match "new-split" s start)
                  (setq splits (1+ splits)
                        start (match-end 0)))
                (should (>= splits 3)))
              ;; Three send commands carrying emacsclient should have been issued.
              (let ((sends 0)
                    (start 0))
                (while (string-match "send" s start)
                  (setq sends (1+ sends)
                        start (match-end 0)))
                (should (= sends 3)))
              (should (string-match-p "emacsclient" s))
              ;; Per F5: panes are bound to the WORKSPACE project, not the
              ;; frame-local one. The cmux mock's `current-workspace' returns
              ;; `name=foo' so the emacsclient command must reference "foo".
              ;; After the shell-quote-argument fix, the lisp form is
              ;; backslash-escaped on POSIX. Accept either quoting form.
              (should (string-match-p
                       "projects-project\\(\\\\ \\| \\)\\.\\(\\\\ \\| \\)\\\\?\"foo\\\\?\""
                       s)))))
      (remhash "theta" projects--table)
      (delete-file capture))))

(ert-deftest projects-cmux/browse-url-opens-cmux-pane ()
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir)))
    (unwind-protect
        (progn
          (projects-cmux--browse-url "https://example.com/x")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              (should (string-match-p "new-surface" s))
              (should (string-match-p "--type\tbrowser" s))
              (should (string-match-p "--url\thttps://example.com/x" s)))))
      (delete-file capture))))

(ert-deftest projects-cmux/browse-url-rejects-unsupported-schemes ()
  "javascript:/data:/custom URLs must NOT be passed to cmux.
file:/// IS allowed (markdown-preview export uses it)."
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir)))
    (unwind-protect
        (progn
          (projects-cmux--browse-url "javascript:alert(1)")
          (projects-cmux--browse-url "data:text/html,<h1>x</h1>")
          (should (= 0 (nth 7 (file-attributes capture))))
          ;; allowed schemes call cmux: mailto and file:///
          (projects-cmux--browse-url "mailto:foo@example.com")
          (projects-cmux--browse-url "file:///tmp/preview.html")
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((s (buffer-string)))
              (should (string-match-p "mailto:foo@example.com" s))
              (should (string-match-p "file:///tmp/preview.html" s)))))
      (delete-file capture))))

(ert-deftest projects-cmux/call-survives-missing-cmux ()
  "When cmux is absent, projects-cmux--call must return non-zero, not error."
  (let* ((projects-cmux--cmux-command "/definitely/not/a/real/cmux/binary"))
    (let ((exit (projects-cmux--call "new-workspace" "--name" "x")))
      (should (integerp exit))
      (should (not (zerop exit))))))

(ert-deftest projects-cmux/stubs-are-defined ()
  "Public surface stubs must exist and be callable as no-ops."
  (should (fboundp 'projects-restore))
  (should (fboundp 'projects-save))
  (should (fboundp 'projects-show-info))
  (should (fboundp 'projects-switch-buffer))
  (should (fboundp 'projects-switch-dispatch))
  (should (fboundp 'projects-move-buffer))
  (should (fboundp 'projects--create-info-buffer))
  (should (fboundp 'projects--tab-bar-format))
  (should (fboundp 'projects--ibuffer-setup))
  (should (fboundp 'projects-register-buffer))
  (should (fboundp 'projects-multi-project-view-p))
  (should (fboundp 'projects-current-window-project))
  (should (fboundp 'projects-clone-from-git))
  ;; calling no-arg stubs must not error
  (should-not (projects--tab-bar-format))
  (should-not (projects--ibuffer-setup))
  (should-not (projects-multi-project-view-p))
  (should-not (projects-register-buffer (current-buffer)))
  ;; info buffer returns a live buffer named with the project
  (let ((buf (projects--create-info-buffer "ws-test")))
    (should (bufferp buf))
    (should (equal (buffer-name buf) "*project: ws-test*"))
    (kill-buffer buf)))

(ert-deftest projects-cmux/save-restore-round-trip ()
  "projects-save writes the table; projects-restore reads it back."
  (let* ((tmp (make-temp-file "cmux-session-" nil ".el"))
         (projects--save-file tmp))
    (unwind-protect
        (progn
          (clrhash projects--table)
          (puthash "alpha" (list :dir "/tmp/alpha/" :buffers nil
                                 :files nil :switch-time 1.0)
                   projects--table)
          (puthash "beta" (list :dir "/tmp/beta/" :buffers nil
                                :files nil :switch-time 2.0)
                   projects--table)
          (setq projects--current "beta")
          (projects-save)
          (should (file-exists-p tmp))
          (clrhash projects--table)
          (setq projects--current nil)
          (should (= 2 (projects-restore tmp)))
          (should (gethash "alpha" projects--table))
          (should (gethash "beta" projects--table))
          (should (equal projects--current "beta")))
      (clrhash projects--table)
      (setq projects--current nil)
      (delete-file tmp))))

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
                                       projects-cmux-test--dir)))
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

(ert-deftest projects-cmux/create-rejects-shell-metacharacters ()
  "projects-create must reject names containing shell metacharacters
and must NOT invoke cmux for any rejected name."
  (let* ((capture (make-temp-file "cmux-cap"))
         (process-environment (cons (concat "CAPTURE_FILE=" capture)
                                    process-environment))
         (projects-cmux--cmux-command (expand-file-name
                                       "../tests/fixtures/cmux-mock.sh"
                                       projects-cmux-test--dir))
         (good-dir (file-name-as-directory (make-temp-file "cmux-bad-" t))))
    (unwind-protect
        (dolist (bad '("bad';rm -rf /"
                       "bad;ls"
                       "bad$(whoami)"
                       "bad`id`"
                       "bad|cat"
                       "bad&echo"
                       "bad\nname"))
          (should-error (projects-create bad good-dir) :type 'user-error)
          (should-not (gethash bad projects--table))
          ;; capture must remain empty across all rejections
          (should (= 0 (nth 7 (file-attributes capture)))))
      (when (file-directory-p good-dir)
        (delete-directory good-dir t))
      (delete-file capture))))

(ert-deftest projects-cmux/emacsclient-command-shell-quotes-name ()
  "projects-cmux--emacsclient-command must wrap the -F payload in shell
quoting so embedded shell metacharacters cannot escape. Verifies via a
round-trip through the shell that the quoted argv parses back to the
original lisp form."
  (let* ((cmd (projects-cmux--emacsclient-command "benign"))
         ;; The captured output of `set -- <quoted args>; printf ...` should
         ;; round-trip the original lisp payload.
         (rt (with-temp-buffer
               (let ((exit (call-process
                            "/bin/sh" nil (current-buffer) nil
                            "-c"
                            (format "set -- %s; printf '%%s' \"$6\""
                                    cmd))))
                 (and (zerop exit) (buffer-string))))))
    (should (stringp cmd))
    (should (string-prefix-p "emacsclient" cmd))
    (should (equal rt "((projects-project . \"benign\"))"))))

(provide 'test-projects-cmux)
;;; test-projects-cmux.el ends here
