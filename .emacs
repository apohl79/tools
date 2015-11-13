;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; Load stuff
(add-to-list 'load-path "~/tools/emacs.d")

; Load yasnippet only on my OS X devbox
;(when (eq system-type 'darwin)
;  (add-to-list 'load-path "~/tools/emacs.d/yasnippet")
;  (require 'yasnippet)
;  (yas-global-mode 1))

(add-to-list 'load-path "~/tools/emacs.d/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

(require 'highlight-current-line)
(highlight-current-line-on t)
(set-face-background 'highlight-current-line-face "#222")

;; Enable package melpa package repo
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;;  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  )

;; uncomment this line to disable loading of "default.el" at startup
; (setq inhibit-default-init t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; show line of file in status line
(setq line-number-mode t)

;; display the column of point in mode line
(setq column-number-mode t)

;; line numbers
(global-linum-mode 1)

;; replace active region just by typing
(delete-selection-mode 1)

;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)

;; disabling backup files
(setq efs-make-backup-file nil)
(setq make-backup-files nil)

;; turn on font-lock mode
(global-font-lock-mode t)
;; enable visual feedback on selections
(setq-default transient-mark-mode t)

;; always end a file with a newline
(setq require-final-newline t)

;; stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; Window and font size
(when window-system
  ;; enable wheelmouse support by default
  (mwheel-install)
  ;; turn of the tool-bar
  (tool-bar-mode -1)
  ;; font-size
  ;(set-default-font "9x15")
  ;; use extended compound-text coding for X clipboard
  (set-selection-coding-system 'compound-text-with-extensions)
  ;(set-face-background 'default "#ffffff")
  ;(set-frame-size (selected-frame) 199 60)
  )
;;(set-frame-height (selected-frame) 200))

;; special key bindings for OS X
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  (when window-system
    (x-focus-frame nil)
    (set-frame-font "Ubuntu Mono-15"))
  )
(message "system-type: %s" system-type)
(when window-system
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Get rid of startup message
(setq inhibit-startup-message t)

;; turn of the menu bar
(menu-bar-mode -1)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; default indention
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

(setq kill-whole-line t)
(setq diff-switches nil)

;; Don't ask for reloading buffer.
(setq revert-without-query '(".*"))

;; Persist the mini-buffer history accross sessions
(savehist-mode 1)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


;; Key bindings
(global-set-key [M-left] 'hide-subtree)
(global-set-key [M-right] 'show-children)
(global-set-key [M-up] 'hide-other)
(global-set-key [M-down] 'show-all)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key "\C-xre" 'replace-regexp)
(global-set-key [C-return] 'newline-and-indent)
(global-set-key "\C-cc" 'dabbrev-expand)
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cu" 'upcase-region)
(global-set-key "\C-cd" 'downcase-region)
(global-set-key "\C-cf" 'find-file-at-point)
(global-set-key (kbd "C-c C-a") 'auto-fill-mode)
(global-set-key "\C-cj" 'set-justification-left)
(global-set-key "\M-g" 'goto-line)

;;(setq compile-command '("make "))
;(global-set-key "\C-co" 'compile)
;(global-set-key "\C-cr" 'run-cmd)
;(global-set-key "\C-ce" 'next-error)

;; leaving emacs without saving current buffer
(defun save-and-killbuf ()
  "save current buffer and quit"
  (interactive)
  ( if ( not buffer-read-only )
      (save-buffer) )
  (kill-this-buffer))
(global-set-key "\C-x\C-y" 'save-and-killbuf)

;; Rebind Cx-# to also leave the buffer
(defun client-notify-and-killbuf ()
  "Say OK to the emacsclient and kill the buffer"
  (interactive)
  (server-edit)
  ( if ( not buffer-read-only )
      (save-buffer) )
  (kill-this-buffer))
(global-set-key "\C-x\#" 'client-notify-and-killbuf)

;; Walk between the windows
(defun my-previous-window ()
  "Previous window"
  (interactive)
  (other-window -1))
(global-set-key "\C-xp" 'my-previous-window)
(global-set-key "\C-xn" 'other-window)

(defun indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; VI-style matching parenthesis
;;  From Eric Hendrickson edh @ med.umn.edu
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))))
(global-set-key "\C-cp" 'match-paren)

;; C/C++
(load "clang-format" t)
(defun my-c-mode-common-hook ()
  (local-set-key [return] 'newline-and-indent)
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'innamespace 0)
  (setq tab-width 8)
  (setq c-basic-offset 4)
  (outline-minor-mode)
  ; If clang-format is available, use it and deactivate electric chars
  (when clang-format-binary-found
    (message "Using clang-format for C/C++ indention: %s" clang-format-binary)
    ;(setq clang-format-style "{BasedOnStyle: Google, ColumnLimit: 120, IndentWidth: 4, AccessModifierOffset: -2, DerivePointerAlignment: false}")
    (setq clang-format-style "file")
    (add-hook 'c-special-indent-hook 'clang-format-region)
    (c-toggle-electric-state -1)
    ))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c++-mode-hook 'my-c-mode-common-hook)

;; Perl
(defun my-cperl-mode-hook ()
  (local-set-key [return] 'newline-and-indent)
  (setq cperl-indent-level 4)
  (setq cperl-continued-brace-offset -2))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)
;; Use cperl-mode if perl is in the shebang
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; Enable doxygen. Downloaded from https://github.com/TreeRex/doxygen-el
(require 'doxygen nil 'noerror)

(defun etags-tags-completion-table ()
  "make tags completion table,  guess C++ member functions correctly"
  (let ((table (make-vector 511 0)))
    (save-excursion
      (goto-char (point-min))
      ;; This monster regexp matches an etags tag line.
      ;;   \1 is the string to match;
      ;;   \2 is not interesting;
      ;;   \3 is the guessed tag name; XXX guess should be better eg DEFUN
      ;;   \4 is not interesting;
      ;;   \5 is the explicitly-specified tag name.
      ;;   \6 is the line to start searching at;
      ;;   \7 is the char to start searching at.
      (while (re-search-forward
              "^\\(\\([^\177]+[^-a-zA-Z0-9_$\177]+\\)?\\([-a-zA-Z0-9_$?:]+\\)\
\[^-a-zA-Z0-9_$?:\177]*\\)\177\\(\\([^\n\001]+\\)\001\\)?\
\\([0-9]+\\)?,\\([0-9]+\\)?\n"
              nil t)
        (intern (if (match-beginning 5)
                    ;; There is an explicit tag name.
                    (buffer-substring (match-beginning 5) (match-end 5))
                  ;; No explicit tag name.  Best guess.
                  (let ((p1 (if (match-beginning 2)
                                (buffer-substring (match-beginning 2) (match-end 2))
                              ""))
                        (p2 (buffer-substring (match-beginning 3) (match-end 3))))
                    (if (string-match "::$" p1)
                        (concat p1 p2)
                      p2)))
                table)))
    table))

(setq find-tag-default-function 'tj-find-tag-default)

(defun tj-find-tag-default ()
  (let ((old-syntax (char-to-string (char-syntax ?:)))
        ret)
    (modify-syntax-entry ?: "w")
    (save-excursion
      (while (looking-at "\\sw\\|\\s_")
        (forward-char 1))
      (if (or (re-search-backward "\\sw\\|\\s_"
                                  (save-excursion (beginning-of-line)
                                                  (point))
                                  t)
              (re-search-forward "\\(\\sw\\|\\s_\\)+"
                                 (save-excursion (end-of-line)
                                                 (point))
                                 t))
          (setq ret (progn (goto-char (match-end 0))
                           (buffer-substring (point)
                                             (progn (forward-sexp -1)
                                                    (while (looking-at
                                                            "\\s'")
                                                      (forward-char
                                                       1))
                                                    (point)))))
        nil))
    (modify-syntax-entry ?: old-syntax)
    ret))
;; End C/C++

;; Some additional keywords
;; C
(font-lock-add-keywords 'c-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
                          ))
;; C++
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\|constexpr\\|thread_local\\|noexcept\\)\\>" . font-lock-keyword-face)
                          ("\\<\\(char16_t\\|char32_t\\)\\>" . font-lock-type-face)
                          ("\\<\\(nullptr\\|true\\|false\\)\\>" . font-lock-constant-face)
                          ("^%\\(typemap\\|define\\|enddef\\|include\\|module\\)" 1 font-lock-builtin-face)
                          ))
;; Perl
(font-lock-add-keywords 'perl-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))
(font-lock-add-keywords 'cperl-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)))
;; latex
(font-lock-add-keywords 'latex-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)))
(font-lock-add-keywords 'latex-math-mode
                        '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
                          ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)))

;; Automatically use the right modes by file extension
(setq auto-mode-alist
      (append '(("\\.app$"                  . c++-mode)
                ("\\.bat$"                  . rexx-mode)        ; to edit batchfiles
                ("\\.bib$"                  . bibtex-mode)      ;
                ("\\.btm$"                  . rexx-mode)
                ("\\.C$"                    . c++-mode)
                ("\\.i$"                    . c++-mode)         ; SWIG: use c++-mode
                ("\\.cc$"                   . c++-mode)
                ("\\.cpp$"                  . c++-mode)
                ("\\.H$"                    . c++-mode)
                ("\\.h$"                    . c++-mode)
                ("\\.hi$"                   . c-mode)
                ("\\.hpp$"                  . c++-mode)
                ("\\.idl$"                  . c++-mode)
                ("\\.c$"                    . c-mode)           ; to edit C code
                ("\\.sqc$"                  . c-mode)           ; NON-Preprocessed C with DB/2 SQL
                ("\\.rc$"                   . c-mode)           ; Files from rc are also smth like c
                ("\\.rci$"                  . c-mode)           ; Files from rc are also smth like c
                ("\\.rcx$"                  . c-mode)           ; Files from rc are also smth like c
                ("\\.cmd$"                  . rexx-mode)        ; to edit REXX-Skripte
                ("\\.c?ps$"                 . postscript-mode)  ; Fuer postscript-files
                ("\\.tex$"                  . latex-mode)       ; tbd
                ("\\.sm$"                   . latex-mode)       ;
                ("\\.sty$"                  . latex-mode)       ;
                ("\\.mak$"                  . makefile-mode)
                ("makefile$"                . makefile-mode)
                ("\\.java$"                 . java-mode)
                ("\\.jav$"                  . java-mode)
                ("\\.py$"                   . python-mode)
                ("\\.xh$"                   . c++-mode)
                ("\\.xih$"                  . c++-mode)
                ("\\.in$"                   . m4-mode)
                ("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode)
                ("\\.pod$"                  . cperl-mode)
                ) auto-mode-alist))

;; ***** Auto complete plugins *****
;; Install:
;; - auto-complete
;; - auto-complete-c-headers
;; - auto-complete-chunk
;; - auto-complete-clang
;; - flycheck
;; - popup
(when (require 'auto-complete-config nil 'noerror)
  ;(add-to-list 'ac-dictionary-directories "~/.emacs.d/AC/ac-dict")
  (require 'auto-complete-c-headers)
;  (require 'auto-complete-clang)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0)
  ;; Add aditional include flags for clang. You can get this list with echo "" | clang++ -v -x c++ -E -
  (setq ac-clang-flags
        (mapcar (lambda (item)(concat "-I" item))
                (split-string
                 "
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../include/c++/v1
/usr/local/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/../lib/clang/6.0/include
/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
/usr/include
/System/Library/Frameworks
/Library/Frameworks
"
                 )
                )
        )
  (setq ac-clang-flags (cons "-std=c++11" ac-clang-flags))

  ;; rebind completion key
  ;(global-unset-key "\C-cc")
  (define-key ac-mode-map "\C-cx" 'auto-complete)
  
  (defun my-ac-config ()
    (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t))
  (defun my-ac-cc-mode-setup ()
    (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  ;; ac-source-gtags
  (my-ac-config)
  
  ;; Make newline-and-indent work with the auto-complete popup
  (defvar my-backup-return-binding nil)
  (defun my-pre-auto-complete (&optional sources)
    (setq my-backup-return-binding (local-key-binding [return]))
    (when my-backup-return-binding
      (local-unset-key [return])))
  (defun my-post-auto-complete ()
    (when my-backup-return-binding
      (local-set-key [return] my-backup-return-binding)
      (setq my-backup-return-binding nil)))
  (advice-add 'auto-complete :after #'my-pre-auto-complete)
  (advice-add 'ac-complete :after #'my-post-auto-complete)
  (advice-add 'ac-abort :after #'my-post-auto-complete)
  )

;; ***** CMake font lock plugin *****
;; Install:
;; - cmake-font-lock
;; - cmake-ide
;; - cmake-mode
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; IDO mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".h" ".cpp" ".c" ".hh" ".lua" ".txt" ".cfg" ".conf" ".cnf" ".xml" ".emacs" ".el"))
(ido-mode 1)
;; IDO for M-x
(global-set-key
 "\M-x"
 (lambda ()
   (interactive)
   (call-interactively
    (intern
     (ido-completing-read
      "M-x "
      (all-completions "" obarray 'commandp))))))

;; activate ecb
(when (require 'ecb nil 'noerror)
  (setq ecb-auto-activate t)
  (setq ecb-tip-of-the-day nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-name "left8")
 '(ecb-layout-window-sizes
   (quote
    (("left8"
      (ecb-directories-buffer-name 0.1365079365079365 . 0.2839506172839506)
      (ecb-sources-buffer-name 0.1365079365079365 . 0.2222222222222222)
      (ecb-methods-buffer-name 0.1365079365079365 . 0.2839506172839506)
      (ecb-history-buffer-name 0.1365079365079365 . 0.19753086419753085))
     ("left3"
      (ecb-directories-buffer-name 0.13333333333333333 . 0.35802469135802467)
      (ecb-sources-buffer-name 0.13333333333333333 . 0.32098765432098764)
      (ecb-methods-buffer-name 0.13333333333333333 . 0.30864197530864196)))))
 '(ecb-mouse-click-destination (quote last-point))
 '(ecb-options-version "2.40")
 '(ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
 '(ecb-source-path (quote (("~/workspace/" "/"))))
 '(package-selected-packages
   (quote
    (lua-mode flycheck ecb cmake-ide cmake-font-lock auto-complete-clang auto-complete-chunk auto-complete-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((t (:background "cornflower blue" :foreground "black")))))
