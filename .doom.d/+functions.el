;;; +functions.el -*- lexical-binding: t; -*-

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; keep flycheck discabled for now as it seems broken....
;(defun my-keep-flycheck-off () (flycheck-mode -1))
;(add-hook 'c++-mode-hook #'my-keep-flycheck-off)

;; indent via clang-format
(load! "clang-format")
(defun my-c-mode-common-hook ()
  (local-set-key [return] 'newline-and-indent)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (setq tab-width 8)
  (setq c-basic-offset 4)
  (outline-minor-mode)
  ; If clang-format is available, use it and deactivate electric chars
  (when clang-format-binary-found
    ;;(setq clang-format-style "{BasedOnStyle: Google, ColumnLimit: 120, IndentWidth: 4, AccessModifierOffset: -2, DerivePointerAlignment: false}")
    ;; Auto indent via clang-format
    (add-hook 'c-special-indent-hook
              (lambda ()
                (interactive)
                (setq my-char-pos (buffer-substring-no-properties (point) (1+ (point))))
                (let ((beg (if mark-active (region-beginning)
                             (min (line-beginning-position) (1- (point-max)))))
                      (end (if mark-active (region-end)
                             (line-end-position))))
                  (when (string-match-p "[^ ]" (buffer-substring-no-properties beg end)) ; ignore empty lines
                    (when (not (equal "}" my-char-pos)) ; allow to move closing }
                      (when (not (equal ")" my-char-pos)) ; allow to move closing )
                        (when (not (equal "]" my-char-pos)) ; allow to move closing ]
                          (clang-format-region beg end))))))))
    (c-toggle-electric-state -1)))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)

;; VI-style matching parenthesis
;;  From Eric Hendrickson edh @ med.umn.edu
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))))

;; Switch between header and implementation
(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h" ".hpp" ".ipp"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.cc\\'" (".h" ".hh"))
    ("\\.mm\\'" (".h"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".cpp" ".cc" ".cxx" ".c" ".mm"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

;; disable company for bash
(defun my-adjust-org-company-backends ()
  (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
  (setq-local company-backends nil))
(add-hook! sh-mode (my-adjust-org-company-backends))

;; window selection for search links, error messages, help etc
;; https://e17i.github.io/articles-emacs-display-1/
(defvar dedicated-other-window nil)
(defvar dedication-count)

;(defun dedicate-window (arg)
;  "Dedicate the currently selected window as 'other' window. When
;called with a =C-0= prefix arg, releases the previously set
;window and reverts to the default window selection behaviour. Default is a
;one-time dedication, use =C-u= for unlimited."
;  (interactive "p")
;  (setq dedicated-other-window
;        (if (eq arg 0)
;            nil
;          (selected-window)))
;  (setq dedication-count
;        (if (eq 4 arg) nil arg))
;  (if dedicated-other-window
;      (message "window dedicated")
;    (message "dedication removed")))

(defun dedicate-window ()
  "Dedicate the currently selected window as 'other' window. When
called with a =C-0= prefix arg, releases the previously set
window and reverts to the default window selection behaviour. Default is a
one-time dedication, use =C-u= for unlimited."
  (interactive)
  (setq dedicated-other-window (selected-window))
  (message "window dedicated"))

(defun window-update-buffer (buffer window type alist)
  "display buffer in window, recording type then apply height"
  (if (window--display-buffer buffer window type alist)
      (window-apply-height-fnc window alist)))

(defun window-apply-height-fnc (window alist)
  "rudimentary alist parser just accepting a height fnc for now.."
  (let ((height (cdr (assq 'window-height alist))))
    (if (functionp height)
        (ignore-errors (funcall height window)))
    window))

(defun display-buffer-dedicated-window (buffer alist)
  "Display pop-up-buffer in the dedicated other window, if one is
selected. If none is selected, revert to the default behaviour."
  (if (and dedicated-other-window
           (window-live-p dedicated-other-window))
      (prog1
          (window-update-buffer buffer dedicated-other-window 'reuse alist)
        )))

(defun my-setup-ide ()
  "My IDE setup."
  (interactive)
  ;(ace-window 0)
  (savehist-save)
  ;(split-window-below -15) ; bottom terminal
  (split-window-right) ; second editor
  ;(other-window 2)
  ;(split-window-right)
  ;(switch-to-buffer (concat "*compilation*<" (projectile-project-name) ">"))
  ;(compilation-setup t)
  ;(compilation-mode)
  ;(setq compilation-scroll-output 'next-error)
  ;(setq compilation-skip-threshold 2)
  ;(other-window 1)
  ;(comint-run "lldb")
  ;(switch-to-buffer "*vterm*")
  ;(vterm)
  ;(treemacs)
  ;(treemacs-display-current-project-exclusively)
  (treemacs-add-and-display-current-project-exclusively)
  (treemacs-project-follow-mode 1)
  (treemacs-follow-mode 1)
  (treemacs--set-width 45)
  (other-window 1)
  (dedicate-window)
  (bury-successful-compilation-turn-on)
  )

(defun my-setup-ide-small ()
  "My IDE setup for smalle screens."
  (interactive)
  (savehist-save)
  (dedicate-window)
  ;(split-window-below -15) ; bottom terminal
  ;(split-window-right) ; second editor
  ;(other-window 2)
  ;(split-window-right)
  ;(switch-to-buffer (concat "*compilation*<" (projectile-project-name) ">"))
  ;(compilation-setup t)
  ;(compilation-mode)
  (setq compilation-scroll-output 'next-error)
  (setq compilation-skip-threshold 2)
  ;(other-window 1)
  ;(comint-run "lldb")
  ;(switch-to-buffer "*vterm*")
  ;(vterm)
  ;(treemacs)
  (treemacs-add-and-display-current-project-exclusively)
  (treemacs-follow-mode 1)
  (other-window 1)
  (bury-successful-compilation-turn-on)
  )

(defun my-setup-ide-nocompile ()
  "My IDE work setup."
  (interactive)
  (ace-window 0)
  (dedicate-window (nth 1 (window-list)))
  (treemacs)
  )

(defun my-compile ()
  "My compile command"
  (interactive)
  (dedicate-window (nth 1 (window-list)))
  (select-window (get-buffer-window (concat "*compilation*<" (projectile-project-name) ">")))
  (+ivy/compile)
  )

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))
