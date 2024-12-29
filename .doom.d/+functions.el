;;; +functions.el -*- lexical-binding: t; -*-

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; leaving emacs without saving current buffer
(defun save-and-killbuf ()
  "save current buffer and quit"
  (interactive)
  ( if ( not buffer-read-only )
      (save-buffer) )
  (kill-this-buffer))

;; Walk between the windows
(defun my-previous-window ()
  "Previous window"
  (interactive)
  (other-window -1))

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
;;; (defun my-adjust-org-company-backends ()
;;;   (remove-hook 'after-change-major-mode-hook '+company-init-backends-h)
;;;   (setq-local company-backends nil))
;;; (add-hook! sh-mode (my-adjust-org-company-backends))

;; Auto hide the compilation buffer
;; based on: https://stackoverflow.com/questions/11043004/emacs-compile-buffer-auto-close
(add-hook 'compilation-start-hook 'compilation-started)
(add-hook 'compilation-finish-functions 'hide-compile-buffer-if-successful)

(defcustom auto-hide-compile-buffer-delay 1
    "Time in seconds before auto hiding compile buffer."
    :group 'compilation
    :type 'number)

(defun hide-compile-buffer-if-successful (buffer string)
  (setq compilation-total-time (time-subtract nil compilation-start-time))
  (setq time-str (concat " (Time: " (format-time-string "%s.%3N" compilation-total-time) "s)"))

  (if
      (with-current-buffer buffer
        (setq warnings (eval compilation-num-warnings-found))
        (setq warnings-str (concat " (Warnings: " (number-to-string warnings) ")"))
        (setq errors (eval compilation-num-errors-found))
        (setq errors-str (concat " (Errors: " (number-to-string errors) ")"))

        (if (and (eq errors 0) (string-prefix-p "finished" string)) nil t))

      ;; If errors or non-zero exit code
      (message (concat "Compiled with Errors" warnings-str errors-str time-str))

    ;; If compiled successfully or with warnings
    (progn
      (bury-buffer buffer)
      (run-with-timer auto-hide-compile-buffer-delay nil 'delete-window (get-buffer-window buffer 'visible))
      (message (concat "Compiled Successfully" warnings-str errors-str time-str)))))

(make-variable-buffer-local 'compilation-start-time)

(defun compilation-started (proc)
  (setq compilation-start-time (current-time)))

(defun my-quickload-session ()
  "Reload the last session with `doom/quickload-session` passing `t`."
  (interactive)
  (doom/quickload-session t))

(defun remove-treemacs-image-icons ()
  "Replace all image (png/svg) icons in treemacs with font based icons."
  ;; Replace all special dir icons
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("src-closed" "test-closed" "tmp-closed" "temp-closed" "build-closed"
                                     "bin-closed" "git-closed" "github-closed" "public-closed" "private-closed"
                                     "screenshot-closed" "icons-closed" "readme-closed" "docs-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("src-open" "test-open" "tmp-open" "temp-open" "build-open" "bin-open"
                                     "git-open" "github-open" "public-open" "private-open" "screenshot-open"
                                     "icons-open" "readme-open" "docs-open"))
  ;; Set some script/config icons
  (treemacs-create-icon :icon (propertize " 	󱆃	" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("zshrc" "bash" "bash_profile" "bash_login" "bash_aliases" "profile"))
  ;; Run over all theme icons and replace the remaining image icons with the fallback file icon.
  (let ((icons (treemacs-theme->gui-icons treemacs--current-theme))
        (def-icon (gethash 'fallback (treemacs-theme->gui-icons treemacs--current-theme))))
    (maphash
     (lambda (k v)
       (when (imagep (get-text-property 0 'display v))
         (puthash k def-icon icons)
         ))
     icons)))
