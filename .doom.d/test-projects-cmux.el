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

(ert-deftest projects-cmux/create-calls-cmux-and-records ()
  ;; The plan's test body uses `/tmp/beta/' as the project directory. The
  ;; sandbox the test agent runs in disallows writing to `/tmp' (only
  ;; `$TMPDIR' and project paths are writable), so we substitute a temp
  ;; directory under `temporary-file-directory'. The contract being verified
  ;; is the same: `projects-create' records the project in `projects--table'
  ;; and invokes cmux with `new-workspace --name ... --cwd ... --command
  ;; <emacsclient ... projects-project ... "beta">'.
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
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((line (buffer-string)))
              (should (string-match-p "new-workspace" line))
              (should (string-match-p "--name\tbeta" line))
              (should (string-match-p (concat "--cwd\t"
                                              (regexp-quote beta-dir))
                                      line))
              (should (string-match-p "--command\t" line))
              (should (string-match-p "emacsclient" line))
              (should (string-match-p "projects-project . \"beta\"" line)))))
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

(ert-deftest projects-cmux/delete-calls-cmux-and-removes ()
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
          (with-temp-buffer
            (insert-file-contents capture)
            (let ((line (buffer-string)))
              (should (string-match-p "close-workspace" line))
              (should (string-match-p "--workspace\tepsilon" line)))))
      (remhash "epsilon" projects--table)
      (delete-file capture))))

(ert-deftest projects-cmux/rename-rewrites-frames ()
  ;; Batch-mode adaptation: use (selected-frame) instead of (make-frame ...)
  ;; per Task 7's deviation digest.
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
          (with-temp-buffer
            (insert-file-contents capture)
            (should (string-match-p "rename-workspace" (buffer-string)))))
      (set-frame-parameter frame 'projects-project saved-param)
      (remhash "zeta" projects--table)
      (remhash "eta" projects--table)
      (delete-file capture))))

(provide 'test-projects-cmux)
;;; test-projects-cmux.el ends here
