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

(provide 'test-projects-cmux)
;;; test-projects-cmux.el ends here
