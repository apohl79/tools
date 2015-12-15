;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

;; Define required packages
(setq package-list
      '(cmake-font-lock
        cmake-ide
        cmake-mode
        ecb
        flycheck
        lua-mode
        async
        company
        company-c-headers
        function-args
        helm
        helm-core
        helm-flycheck
        helm-git
        helm-gtags
        mustard-theme
        smart-mode-line
        smart-mode-line-powerline-theme
        yasnippet
        yatemplate))

;; Enable package melpa package repo
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-initialize)
  ;; Create a function that installs all need packages
  (defun install-my-packages ()
    (interactive)
    (when (>= emacs-major-version 24)
      (message "Installing packages...")
      (unless package-archive-contents
        (package-refresh-contents))
      (dolist (package package-list)
        (unless (package-installed-p package)
          (package-install package)))
      (message "Installing packages... done"))))

;; Load stuff
(add-to-list 'load-path "~/tools/emacs.d")

(load-theme #'mustard t)

(when (require 'highlight-current-line)
  (highlight-current-line-on t))

(when (require 'smart-mode-line nil 'noerror)
  (setq sml/no-confirm-load-theme t)
  ;(setq sml/theme 'dark)
  (setq sml/theme 'powerline)
  (sml/setup))

;; uncomment this line to disable loading of "default.el" at startup
; (setq inhibit-default-init t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; show line of file in status line
(setq line-number-mode t)
(setq linum-format " %d")

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
  ;; turn off the tool-bar
  (tool-bar-mode -1)
  ;; turn off scrollbars
  (toggle-scroll-bar -1)
  ;; turn off the menu bar
  ;(menu-bar-mode -1)
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
  (setq ns-use-srgb-colorspace nil)
  (when window-system
    (x-focus-frame nil)
    (set-frame-font "Monaco-13")))

(when window-system
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

;; Get rid of startup message
(setq inhibit-startup-message t)

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
(global-set-key "\C-cx" 'dabbrev-expand)
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

;; CMake font lock plugin
;; Install:
;; - cmake-font-lock
;; - cmake-ide
;; - cmake-mode
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)

;; Helm
(when (require 'helm nil 'noerror)
  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "M-x") 'helm-M-x)

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-y")  'helm-select-action) ; list actions using C-y

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-prefix-key "\C-cg"
        helm-gtags-suggested-key-mapping t)

  (require 'helm-gtags)
  ;; Enable helm-gtags-mode
  (add-hook 'dired-mode-hook 'helm-gtags-mode)
  (add-hook 'eshell-mode-hook 'helm-gtags-mode)
  (add-hook 'c-mode-hook 'helm-gtags-mode)
  (add-hook 'c++-mode-hook 'helm-gtags-mode)
  (add-hook 'asm-mode-hook 'helm-gtags-mode)

  (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
  (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
  (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
  (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
  (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

  ;; Change some colors
  (set-face-attribute 'helm-selection nil :background "#a1d7f2" :foreground "black")
  (set-face-attribute 'helm-ff-directory nil :background "#191919" :foreground "#a1d7f2")
  (set-face-attribute 'helm-buffer-directory nil :background "#191919" :foreground "#a1d7f2")
  (set-face-attribute 'helm-bookmark-directory nil :background "#191919" :foreground "#a1d7f2")

  (helm-mode 1))

;; Install company, company-c-headers
(when (require 'company nil 'noerror)
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-backends (delete 'company-clang company-backends))
  (add-to-list 'company-backends 'company-c-headers) ; c header files completion
  ;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.2.1/") ; c++ header files completion (osx)
  ;(add-to-list 'company-c-headers-path-system "/usr/include/c++/4.9/") ; c++ header files completion (linux)
  (global-set-key (kbd "C-c c") 'company-complete))

(when (require 'semantic nil 'noerror)
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (semantic-add-system-include "/usr/include/boost" 'c++-mode)
  (semantic-add-system-include "/usr/local/include/boost" 'c++-mode)
  (global-set-key (kbd "C-c c") 'company-semantic)
  ; show func header at the top if the func is longer than one screen
  (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (semantic-mode 1))

;; Install function-args
(when (require 'function-args nil 'noerror)
  (fa-config-default)
  (add-hook 'c-mode-hook
            (lambda ()
              (define-key c-mode-map  [(control tab)] 'moo-complete)
              (define-key c-mode-map (kbd "M-o")  'fa-show)))
  (add-hook 'c++-mode-hook
            (lambda ()
              (define-key c++-mode-map  [(control tab)] 'moo-complete)
              (define-key c++-mode-map (kbd "M-o")  'fa-show))))

;; show unncessary whitespaces
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; activate ecb
(when (require 'ecb nil 'noerror)
  (setq ecb-auto-activate t)
  (setq ecb-tip-of-the-day nil))

;; auto-insert mode
(add-hook 'find-file-hook 'auto-insert)

;; yasnippet support
(when (require 'yasnippet)
  (yas-global-mode 1))

;; load file templates from tools/emacs.d/templates (needs to be symlinked into ~/.emacs.d)
(when (require 'yatemplate nil 'noerror)
  (yatemplate-fill-alist))

  (semantic-add-system-include "/usr/include/boost" 'c++-mode)
  (semantic-add-system-include "/usr/local/include/boost" 'c++-mode)

;; flycheck
;(add-hook 'after-init-hook #'global-flycheck-mode)
;(add-hook 'c++-mode-hook
;          (lambda ()
;            (setq flycheck-clang-language-standard "c++1y")
;            (setq flycheck-clang-include-path '("/Users/andreas/workspace/nghttp2/src/includes"))
;            ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("79a3f477ac0cb4a106f78b6109614e991564a5c2467c36e6e854d4bc1102e178" "2dd32048690787844d8cba601ed3dd8b2f419e9bd985898d0c3792671a05b96b" default)))
 '(ecb-layout-name "left2")
 '(ecb-layout-window-sizes
   (quote
    (("left2"
      (ecb-directories-buffer-name 0.10793650793650794 . 0.43209876543209874)
      (ecb-sources-buffer-name 0.10793650793650794 . 0.5555555555555556))
     ("left8"
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
 '(find-tag-default-function (quote tj-find-tag-default))
 '(global-semantic-stickyfunc-mode t)
 '(package-selected-packages
   (quote
    (cmake-font-lock lua-mode flycheck ecb cmake-ide auto-complete-clang auto-complete-chunk auto-complete-c-headers)))
 '(temp-buffer-show-function (quote ecb-temp-buffer-show-function-emacs))
 '(tool-bar-mode nil)
 '(transient-mark-mode (quote (only . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-default-highlight-face ((t (:background "#a1d7f2" :foreground "black"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "dim gray"))))
 '(font-lock-comment-face ((t (:foreground "dim gray"))))
 '(font-lock-function-name-face ((t (:foreground "#00cc60"))))
 '(font-lock-type-face ((t (:foreground "#f7c527" :underline nil :weight bold))))
 '(font-lock-warning-face ((t (:background "dark red" :foreground "#F8F8F0"))))
 '(fringe ((t (:background "#232323"))))
 '(highlight-current-line-face ((t (:background "#232323"))))
 '(linum ((t (:background "#232323"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#232323" :foreground "gray60" :inverse-video nil :box nil :weight light)))))
