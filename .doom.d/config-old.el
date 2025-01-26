;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Andreas Pohl"
      user-mail-address "pohl@e47.org")

;(setq debug-on-error t)

(load! "+functions")
(load! "+bindings")

;; -------------------------------------------------------------------------------------------------
;; EMAIL
;; -------------------------------------------------------------------------------------------------

(use-package! mu4e
  ;; Fix the setup of org-msg for mu4e
  :init (add-hook 'org-msg-mode-hook
                  (lambda ()
                    (org-msg-mode-mu4e)
                    (org-msg-edit-mode-mu4e)
                    ;; this fixes the problem of not closing the edit buffer properly
                    (add-hook 'message-sent-hook
                              (lambda ()
                                (my-message-kill-buffer-no-query)
                                (mu4e-compose-post-restore-window-configuration)))))
  :config
  (setq mail-user-agent 'mu4e-user-agent ; important for org-msg
        mu4e-view-show-images t
        mu4e-compose-signature-auto-include nil
        mu4e-use-fancy-chars t
        mu4e-split-view 'vertical
        mu4e-headers-visible-columns 120

        ; send setup, see ~/.msmtprc
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail

        ; receive setup, see ~/.mbsyncrc
        mu4e-get-mail-command "mbsync --config ~/.mbsyncrc e47"
        mu4e-update-interval 300
        mu4e-headers-auto-update t

        ; bookmarks
        mu4e-bookmarks '((:name "Unread"
                          :query "maildir:/INBOX AND flag:unread"
                          :key ?i
                          :favorite t))

        ; dirs
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-maildir-shortcuts '((:maildir "/INBOX" :key ?i)
                                 (:maildir "/Sent" :key ?s)
                                 (:maildir "/Drafts" :key ?d)
                                 (:maildir "/Trash" :key ?t)
                                 (:maildir "/Junk" :key ?j)
                                 (:maildir "/Spam" :key ?g :hide-unread t))

        ; avoid replying to ourselves
        mu4e-compose-reply-ignore-address '("no-?reply" "pohl@e47.org")))

(use-package! mu4e-views
  :defer nil
  :after mu4e
  :config
  (setq mu4e-views-default-view-method "html" ;; make xwidgets default
        ;; when pressing n and p stay in the current window
        mu4e-views-next-previous-message-behaviour 'stick-to-current-window
        ;; automatically open messages when moving in the headers view
        mu4e-views-auto-view-selected-message t)
  (mu4e-views-mu4e-use-view-msg-method "html")) ;; select the default

;; -------------------------------------------------------------------------------------------------
;; THEME
;; -------------------------------------------------------------------------------------------------

(defvar my-fixed-font "Iosevka Comfy")
(defvar my-variable-font "Roboto")

(setq doom-font
      (font-spec :family my-fixed-font :size 13)
      doom-variable-pitch-font
      (font-spec :family my-variable-font :size 13))

(setq doom-font-increment 1)

;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-city-lights)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
;; Always fixed font even in variable-pitch-mode
(set-face-attribute 'line-number nil :font my-fixed-font)
(set-face-attribute 'line-number-current-line nil :font my-fixed-font)

;; More colors in treesitter
(setq treesit-font-lock-level 4)

;; Treemacs tweaks
(after! treemacs
  (setq treemacs-width 45)
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1)
  (set-face-attribute 'treemacs-root-face nil :height 1.0)
  ;; treemacs png/svg special icons don't look great, so we patch the icon set
  (add-hook 'treemacs-mode-hook 'my-update-treemacs-icons))

;; Org mode looks
(after! org-mode
  (setq org-support-shift-select t
        org-replace-disputed-keys t))

(use-package! org-modern
  :after org
  :hook ((org-mode . global-org-modern-mode)
         (org-mode . (lambda ()
                       ;; increase line spacing a little
                       (setq-local default-text-properties '(line-spacing 0.1 line-height 1.1)))))
  :config
  (setq org-modern-star 'replace
        org-modern-label-border 0.3
        org-modern-replace-stars "●●●●●"
        org-modern-todo-faces (quote (("WAIT" :inherit org-modern-todo :height 1.2 :foreground "goldenrod")
                                      ("HOLD" :inherit org-modern-todo :height 1.2 :foreground "indian red")
                                      ("DONE" :inherit org-modern-todo :height 1.2 :inverse-video nil
                                       :foreground "white" :distant-foreground "white" :background "grey25"))))
  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font my-variable-font :weight 'bold :height 1.3 :underline t)
  ;; Resize headings
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font my-variable-font :height (cdr face))))

(after! org-modern-faces
  (set-face-attribute 'org-modern-symbol nil :family my-fixed-font))

(use-package! mixed-pitch
  :after org
  :hook (org-mode . mixed-pitch-mode))

;; Maximize at startup and fix title-bar height
(add-hook 'doom-init-ui-hook
  (lambda ()
    ;; enable/disable toolbar mode to set the proper (minimal) titlebar height (macOS)
    (tool-bar-mode 1)
    (tool-bar-mode 0)
    ;; maximize emacs w/o setting a frame property to keep normal macos window management working
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) (- (display-pixel-width) 16) (display-pixel-height) t)))


;; -------------------------------------------------------------------------------------------------
;; GENERAL DEFAULTS
;; -------------------------------------------------------------------------------------------------

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; auto save
;(setq auto-save-default t
;      make-backup-files t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Disable the quit question
(setq confirm-kill-emacs nil)

;; make sure we get asked to accept non-safe local variables from .dir-locals.el files
;(setq enable-local-variables t)
(setq enable-local-variables :all)

;; "ctrl - left click" buffer menu: increase number of items shown
;; set max length of this list. default 20. see next.
(setq mouse-buffer-menu-maxlen 30)
;; set # buffer in a mode before grouping begins. takes precedence over previous
;; set to 1 to always group by mode. default 4
(setq mouse-buffer-menu-mode-mult 1)

;; Set the project name as frame title (window name in macOS)
(setq frame-title-format '("" "%b" (:eval
                                    (let ((project-name (projectile-project-name)))
                                      (unless (string= "-" project-name)
                                        (format " in [%s]" project-name))))))

;; Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
      org-support-shift-select t
      org-replace-disputed-keys t
      org-startup-indented t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Spellchecker
(use-package! jinx
  :hook ((org-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :config
  (setq jinx-languages "en_US de_DE_frami"
        jinx-delay 1.0)
  ; no spell checking in strings
  (add-to-list 'jinx-exclude-faces '(prog-mode font-lock-string-face)))

(after! vertico-multiform ;; if using vertico
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25))))

(set-default 'truncate-lines nil)

;(use-package! consult-omni
;  :after consult
;  :config
;  ;; Load Sources Core code
;  (require 'consult-omni-sources)
;  ;; Load Embark Actions
;  (require 'consult-omni-embark)
;  ;; Only load brave-auto-suggest source
;  ;(require 'consult-omni-brave-autosuggest)
;  (consult-omni-sources-load-modules)
;  ;;; Set your shorthand favorite interactive command
;  (setq consult-omni-default-interactive-command #'consult-omni-brave-autosuggest))

;; Dashboard menu
;(plist-put (alist-get "Reload last session" +doom-dashboard-menu-sections nil nil 'equal)
;           :action doom/quickload-session)

;; Restore last session automatically
(add-hook! 'window-setup-hook #'my-quickload-session)


;; -------------------------------------------------------------------------------------------------
;; CODING - LSP, CODE COMPLETION etc
;; -------------------------------------------------------------------------------------------------

;; (setq company-lsp-enable-snippet t)
;; (after! company
;;   (setq company-idle-delay 0.1
;;         company-minimum-prefix-length 1))
;;
;; ;; Disable dabbrev & ispell completion in org-mode
;; (after! company
;;   (set-company-backend! 'org-mode
;;     '(:separate company-capf company-yasnippet)))

(yas-global-mode 1)

;; Compilation buffer: autosave and stop at the first error and skip warnings
(setq compilation-scroll-output 'next-error
      compilation-skip-threshold 2)

;; Make the git summary line longer
(after! magit
  (setq git-commit-summary-max-length 120))

;; LSP support and code completion
(use-package! lsp-bridge
  :config
  (setq lsp-bridge-enable-log nil
        lsp-bridge-enable-mode-line t
        lsp-bridge-enable-completion-in-string t
        lsp-bridge-enable-hover-diagnostic t
        lsp-bridge-enable-search-words nil
        acm-enable-tabnine t
        acm-backend-yas-candidate-min-length 2)
  (my-enable-global-lsp-bridge-mode))

;; Enable the lsp-bridge flymake backend
(use-package! flymake-bridge
  :after flymake
  :hook (lsp-bridge-mode-hook . flymake-bridge-setup))

;; Disable flymake for elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (flymake-mode -1)))

;; (use-package! lsp-mode
;;   :defer t
;;   :config
;;   (setq lsp-disabled-clients '(ccls)
;;         lsp-idle-delay 0.9
;;         lsp-restart 'auto-restart
;;         lsp-ui-doc-enable nil
;;         ;; Use xcode's clangd
;;         ;lsp-clients-clangd-executable "/Library/Developer/CommandLineTools/usr/bin/clangd"
;;         lsp-clients-clangd-args '("--log=error"
;;                                   "--background-index"
;;                                   "--clang-tidy"
;;                                   "--completion-style=detailed"
;;                                   "--header-insertion=never"
;;                                   "--pretty")
;;         ;; Disable some pygthon warnings
;;         lsp-pylsp-plugins-flake8-ignore "E128,E261,E265,E302,E401,E501,E713,E741"
;;         lsp-pylsp-plugins-pydocstyle-enabled nil
;;         lsp-pylsp-plugins-mccabe-threshold 40
;;         lsp-tailwindcss-add-on-mode t)
;;
;;   ;; Use an alternative typescript lsp, install via npm
;;   ;; npm install -g @vtsls/language-server
;;   ;(lsp-register-client
;;   ; (make-lsp-client
;;   ;  :new-connection (lsp-stdio-connection
;;   ;                   (lambda ()
;;   ;                     `("node" ,(expand-file-name "~/.nvm/versions/node/v20.12.2/bin/vtsls") "--stdio")))
;;   ;  :priority -1
;;   ;  :major-modes '(typescript-mode)
;;   ;  :server-id 'vtsls))
;;   )

;;(add-hook 'typescript-mode-hook
;;          (lambda ()
;;            ;(setq-local lsp-enabled-clients '(eslint tailwindcss ts-ls))
;;            (setq-local lsp-enabled-clients '(ts-ls eslint))
;;            (lsp-deferred)))

;; Set flycheck cpp standard
;(add-hook 'c++-mode-hook
;          (lambda ()
;            (setq flycheck-clang-language-standard "c++17")))

;(add-hook! prog-mode #'flymake-mode)

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
                ("\\.puml$"                 . plantuml-mode)
                ("\\.ino$"                  . c++-mode)
                ("\\.ts$"                   . typescript-mode)
                ("\\.tsx$"                  . typescript-mode)
                ) auto-mode-alist))

;; Disable tree-sitter modes
(add-to-list 'major-mode-remap-alist '(js-ts-mode . js-mode))
(add-to-list 'major-mode-remap-alist '(typescript-ts-mode . typescript-mode))
(add-to-list 'major-mode-remap-alist '(tsx-ts-mode . typescript-mode))

;; Set up default file templates based on the project
(set-file-template! "\\.hpp$" :trigger "__hpp" :mode 'c++-mode)
(set-file-template! "\\.cpp$" :trigger "__cpp" :mode 'c++-mode)
(set-file-template! "syncdna.*\\.hpp$" :trigger "sdna_hpp" :mode 'c++-mode)
(set-file-template! "syncdna.*\\.cpp$" :trigger "sdna_cpp" :mode 'c++-mode)
(set-file-template! "AudioGridder.*\\.hpp$" :trigger "ag_hpp" :mode 'c++-mode)
(set-file-template! "AudioGridder.*\\.cpp$" :trigger "ag_cpp" :mode 'c++-mode)

;; Load dap-mode
(use-package! dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode)
  ;; Enable dap-ui mode for a better experience
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-codelldb)
  (dap-codelldb-setup)

  ;; Register a default debug template for C++ projects
  (dap-register-debug-template
    "C++ LLDB::Run"
    (list :type "lldb"
          :request "launch"
          :name "C++ LLDB::Run"
          :program "${workspaceFolder}/"
          :cwd nil)))

;; Enable vterm-copy-mode automatically when scrolling up
;(advice-add 'set-window-vscroll :after
;            (defun my-vterm-toggle-scroll (&rest _)
;              (when (eq major-mode 'vterm-mode)
;                (if (> (window-end) (buffer-size))
;                    (when vterm-copy-mode (vterm-copy-mode-done nil))
;                  (vterm-copy-mode 1)))))

;; -------------------------------------------------------------------------------------------------
;; LLM SETUP
;; -------------------------------------------------------------------------------------------------

(use-package! elysium
  :defer t
  :custom
  (elysium-window-size 0.45)
  (elysium-window-style 'vertical)
  ; enable smerge-mode explicitely
  :hook (elysium-apply-changes . smerge-start-session))

(use-package! gptel
  :defer t
  :custom
  (gptel-model 'claude-3-5-sonnet-20241022)
  :config
  (setq gptel-default-mode 'org-mode)

  ;; Integrations
  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))

  ;; OpenAI
  (setq! gptel-api-key (read-file-contents "~/.gptel/chatgpt.key"))

  ;; Google
  (defun gptel-gemini-api-key ()
    (read-file-contents "~/.gptel/gemini.key"))
  (gptel-make-gemini "Gemini" :stream t
                     :key #'gptel-gemini-api-key)

  ;; Anthropic (default)
  (defun gptel-claude-api-key ()
    (read-file-contents "~/.gptel/claude.key"))
  (setq gptel-backend
        (gptel-make-anthropic "Claude" :stream t
                              :key #'gptel-claude-api-key)))
