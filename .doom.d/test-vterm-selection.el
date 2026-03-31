;;; test-vterm-selection.el -*- lexical-binding: t; -*-

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

(load-file "/Users/andreas.pohl/tools/.doom.d/+functions.el")

(ert-deftest my/vterm-window-config-reset-skips-copy-mode ()
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (setq vterm-copy-mode t)
    (let ((called nil))
      (cl-letf (((symbol-function 'vterm-reset-cursor-point)
                 (lambda () (setq called t))))
        (my/vterm-reset-cursor-point-maybe)
        (should-not called)))))

(ert-deftest my/vterm-window-config-reset-runs-outside-copy-mode ()
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (setq vterm-copy-mode nil)
    (let ((called nil))
      (cl-letf (((symbol-function 'vterm-reset-cursor-point)
                 (lambda () (setq called t))))
        (my/vterm-reset-cursor-point-maybe)
        (should called)))))

(ert-deftest my/vterm-window-config-reset-ignores-transient-errors ()
  (with-temp-buffer
    (setq major-mode 'vterm-mode)
    (setq vterm-copy-mode nil)
    (cl-letf (((symbol-function 'vterm-reset-cursor-point)
               (lambda () (error "End of buffer"))))
      (should (equal (my/vterm-reset-cursor-point-maybe) nil)))))
