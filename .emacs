;; $Id: .emacs,v 1.6 2008-09-06 00:08:02 osar Exp $
;; Copyright (c) Andreas Pohl

;; --------------------------------------------------------------------------
;; Some default RedHat stuff at the beginning...
;; --------------------------------------------------------------------------
;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" system-name))

;; Red Hat Linux default .emacs initialization file  ; -*- mode: emacs-lisp -*-

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

;; --------------------------------------------------------------------------
;; Now my stuff... 
;; --------------------------------------------------------------------------

;(add-to-list 'load-path "/home/osar/workspace/git-emacs")
;(require 'git-emacs)

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
  (set-face-background 'default "#ffffff")
  (set-frame-size (selected-frame) 199 60))
  ;;(set-frame-height (selected-frame) 200))

;; special key bindings for OS X
(when (eq system-type 'darwin)
  (setq mac-option-modifier nil)
  (setq mac-command-modifier 'meta)
  )

;; Get rid of startup message
(setq inhibit-startup-message t)

;; turn of the menu bar
(menu-bar-mode -1)

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; CEDET stuff
;(setq semantic-load-turn-useful-things-on t)
;(load-file "/usr/share/emacs/site-lisp/cedet/common/cedet.el")
;(require 'semantic-load)
;(setq semanticdb-project-roots
;      (list "~/adtech/NGGoalCache"))
;(setq speedbar-frame-parameters 
;      '((minibuffer) (width . 32) (height . 100) (border-width . 0) (menu-bar-lines . 0)
;	(unsplittable . t)))
;(autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
;(autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)

;; show line numbers
;(load-file "/usr/share/emacs/site-lisp/setnu/setnu.el")
;; for c++
;(add-hook 'c++-mode-hook 'setnu-mode)

;; Some cperl-mode customization
(defun my-cperl-mode-hook ()
  (local-set-key [return] 'newline-and-indent)
  ;; indentation should be 4 (2 is default)
  ;(setq cperl-indent-level 4)
  ;; if()\n{\n
  ;(setq cperl-extra-newline-before-brace t)
  ;; automatic newline and indent
  ;(setq cperl-auto-newline t)
  ;; the brakets should not be indeted
  (setq cperl-continued-brace-offset -2))
(add-hook 'cperl-mode-hook 'my-cperl-mode-hook)

;; Some c-mode customization
(defun my-c-mode-common-hook ()
  (local-set-key [return] 'newline-and-indent)
  ;; GNU style
  ;;(c-set-style "gnu")
  ;; my customizations for all of c-mode and related modes
  (c-set-offset 'substatement-open 0)
  ;; other customizations can go here
  (setq tab-width 4)
  (setq c-basic-offset 4)
  (setq indent-tabs-mode t)
  ;; automatic newline, and hungry mode
  ;;(c-toggle-auto-hungry-state 1)
)
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;(add-hook 'c++-mode-hook 'my-c-mode-common-hook)

(add-hook 'java-mode-hook 'my-java-mode-hook)
(defun my-java-mode-hook ()
  (cond (window-system
         (require 'andersl-java-font-lock)
         (turn-on-font-lock))))

;;
;; TAGS C++ extention to handle name spaces
;;
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

;; end TAGS C++

;;=================
;;===  K E Y S ====
;;=================

;; compile-command
;;(setq compile-command '("make "))
(global-set-key "\C-co" 'compile)
(global-set-key "\C-cr" 'run-cmd)
(global-set-key "\C-ce" 'next-error)

;; Specials fuer die Keyboardsteuerung unter NT:
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)

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

;; Goto-Line also M-g
(global-set-key "\M-g" 'goto-line)


;; Walk between the windows
(defun my-previous-window ()
  "Previous window"
  (interactive) 
  (other-window -1))
(global-set-key "\C-xp" 'my-previous-window)

(global-set-key "\C-xn" 'other-window)


;; Buffer cycling functions
(defun my-unbury-buffer (&optional buf)
  "Select buffer BUF, or the last one in the buffer list.
This function is the opposite of `bury-buffer'."
  (interactive)
  (or buf (setq buf (car (reverse (buffer-list)))))
  (switch-to-buffer buf))

;; some keys I like
(global-set-key "\C-xre" 'replace-regexp)
(global-set-key [C-return] 'newline-and-indent)
;(global-set-key (kbd "C-c C-SPC") 'dabbrev-expand)
(global-set-key "\C-cc" 'dabbrev-expand)
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cu" 'upcase-region)
(global-set-key "\C-cd" 'downcase-region)
(global-set-key "\C-cf" 'find-file-at-point)
(global-set-key (kbd "C-c C-a") 'auto-fill-mode)
(global-set-key "\C-cj" 'set-justification-left)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)


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

;; use cperl-mode if perl is in the #!
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))

;; show line of file in status line
(setq line-number-mode t)

;; display the column of point in mode line
(setq column-number-mode t)

;; tabs (t) or spaces (nil)
(setq-default indent-tabs-mode t)
(setq indent-tabs-mode t)

;; use all the 'Win-keys'
;(pc-selection-mode)

;; Some additional keywords
;;  c
(font-lock-add-keywords 'c-mode
 '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
   ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
   ;("^#[ \t]*\\(import\\|include\\)" . font-lock-function-name-face)
   ))
;;  c++
(font-lock-add-keywords 'c++-mode
 '(("\\<\\(FIXME\\)" 1 font-lock-warning-face prepend)
   ("\\<\\(TODO\\)" 1 font-lock-warning-face prepend)
   ("\\<\\(and\\|or\\|not\\)\\>" . font-lock-keyword-face)
   ;("^#[ \t]*\\(import\\|include\\)" . font-lock-function-name-face)
   ("^%\\(typemap\\|define\\|enddef\\|include\\|module\\)" 1 font-lock-builtin-face)
   ))
;;  perl
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

;; VI-style matching parenthesis
;;  From Eric Hendrickson edh @ med.umn.edu
(defun match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))))
(global-set-key "\C-cp" 'match-paren)
;;Stolen from Robin Socha's .emacs (http://www.socha.net/XEmacs/).

(setq kill-whole-line t)
(setq diff-switches nil) 

;; Don't ask for reloading buffer.
(setq revert-without-query '(".*"))
