;;; test-projects-backup-files.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(defmacro load! (&rest _) nil)
(defmacro after! (&rest body) `(progn ,@(cdr body)))
(defmacro use-package! (&rest _) nil)
(defmacro map! (&rest _) nil)
(defmacro add-hook! (&rest _) nil)
(defmacro undefine-key! (&rest _) nil)
(defmacro set-popup-rule! (&rest _) nil)
(defmacro defadvice! (&rest _) nil)

(defvar doom-data-dir temporary-file-directory)

(defun evil-set-initial-state (&rest _) nil)

(load-file "/Users/andreas.pohl/tools/.doom.d/projects.el")
(remove-hook 'kill-emacs-hook #'projects-save)

(defun projects-test--with-formatted-times (times fn)
  "Call FN while `format-time-string' returns TIMES sequentially."
  (let ((formatted-times times))
    (cl-letf (((symbol-function 'format-time-string)
               (lambda (&rest _args)
                 (prog1 (car formatted-times)
                   (setq formatted-times (cdr formatted-times))))))
      (funcall fn))))

(ert-deftest projects-backup-files-includes-timestamps-in-labels ()
  (let ((projects--save-file "/tmp/session.el")
        (projects--backup-count 2))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path)
                 (member path '("/tmp/session.el"
                                "/tmp/session.el.1"
                                "/tmp/session.el.2"))))
              ((symbol-function 'file-attributes)
               (lambda (path) path))
              ((symbol-function 'file-attribute-modification-time)
               (lambda (path)
                 (pcase path
                   ("/tmp/session.el" (date-to-time "2026-03-31 14:25:00"))
                   ("/tmp/session.el.1" (date-to-time "2026-03-31 13:55:00"))
                   ("/tmp/session.el.2" (date-to-time "2026-03-31 12:25:00")))))
              ((symbol-function 'current-time)
               (lambda () (date-to-time "2026-03-31 14:30:00"))))
      (projects-test--with-formatted-times
       '("2026-03-31 14:25"
         "2026-03-31 13:55"
         "2026-03-31 12:25")
       (lambda ()
         (let ((choices (projects--backup-files)))
           (should (equal (mapcar #'cdr choices)
                          '("/tmp/session.el"
                            "/tmp/session.el.1"
                            "/tmp/session.el.2")))
           (should (string-match-p
                    "current  (session\\.el)"
                    (caar choices)))
           (should (string-match-p
                    "2026-03-31 14:25"
                    (caar choices)))
           (should (string-match-p
                    "5m ago"
                    (caar choices)))
           (should (string-match-p
                    "backup 1 (session\\.el\\.1)"
                    (car (nth 1 choices))))))))))

(ert-deftest projects-backup-files-skips-missing-backups-and-keeps-order ()
  (let ((projects--save-file "/tmp/session.el")
        (projects--backup-count 3))
    (cl-letf (((symbol-function 'file-exists-p)
               (lambda (path)
                 (member path '("/tmp/session.el"
                                "/tmp/session.el.2"))))
              ((symbol-function 'file-attributes)
               (lambda (path) path))
              ((symbol-function 'file-attribute-modification-time)
               (lambda (_path)
                 (date-to-time "2026-03-31 14:25:00")))
              ((symbol-function 'current-time)
               (lambda () (date-to-time "2026-03-31 14:30:00"))))
      (projects-test--with-formatted-times
       '("2026-03-31 14:25"
         "2026-03-31 14:25")
       (lambda ()
         (let ((choices (projects--backup-files)))
           (should (equal (mapcar #'cdr choices)
                          '("/tmp/session.el"
                            "/tmp/session.el.2")))
           (should (= (length choices) 2))))))))
