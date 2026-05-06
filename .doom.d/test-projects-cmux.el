;;; test-projects-cmux.el --- ERT tests for projects-cmux  -*- lexical-binding: t; -*-
(require 'ert)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "projects-cmux.el" dir) nil t))

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

(provide 'test-projects-cmux)
;;; test-projects-cmux.el ends here
