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

;;
;; THEME
;;

(defvar my-fixed-font "Iosevka Comfy")
(defvar my-variable-font "Iosevka Comfy Duo")

(setq doom-font
      (font-spec :family my-fixed-font :size 13)
      doom-variable-pitch-font
      (font-spec :family my-variable-font :size 13))

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
  ;; treemacs png/svg special icons don't look great, so we patch the icon set
  (add-hook 'treemacs-mode-hook 'remove-treemacs-image-icons))

;; Org mode looks
(after! org-mode
  (setq org-support-shift-select t
        org-replace-disputed-keys t))

(after! org-faces
  ;; Resize headings
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil
                        :font my-variable-font
                        :height (cdr face)))
  ;; Make the document title a bit bigger
  (set-face-attribute 'org-document-title nil :font my-variable-font :weight 'bold :height 1.3 :underline t))

(use-package! org-modern
  :after org
  :hook (org-mode . (global-org-modern-mode variable-pitch-mode))
  :config
  ;org-modern-symbol
  (setq org-modern-star 'replace
        org-modern-label-border 0.3))

(after! org-modern-faces
  (set-face-attribute 'org-modern-symbol nil :family my-fixed-font))

;; Maximize at startup and fix title-bar height
(add-hook 'doom-init-ui-hook
  (lambda ()
    ;; enable/disable toolbar mode to set the proper (minimal) titlebar height (macOS)
    (tool-bar-mode 1)
    (tool-bar-mode 0)
    ;; maximize emacs w/o setting a frame property to keep normal macos window management working
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) (- (display-pixel-width) 16) (display-pixel-height) t)))

;;
;; GENERAL DEFAULTS
;;

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

;(plist-put (alist-get "Reload last session" +doom-dashboard-menu-sections nil nil 'equal)
;           :action doom/quickload-session)

;; Spellchecker
(setq ispell-program-name "hunspell")

;; Restore last session automatically
(add-hook! 'window-setup-hook #'my-quickload-session)


;;
;; CODING - LSP, CODE COMPLETION etc
;;

;; an alternative to the standard typescript lsp
;; npm install -g @vtsls/language-server
(use-package! lsp-mode
  :defer t
  :config
  (setq lsp-disabled-clients '(ccls)
        lsp-idle-delay 0.9
        lsp-restart 'auto-restart
        lsp-ui-doc-enable nil
        ;; Use xcode's clangd
        ;lsp-clients-clangd-executable "/Library/Developer/CommandLineTools/usr/bin/clangd"
        lsp-clients-clangd-args '("--log=error"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--pretty")
        ;; Disable some pygthon warnings
        lsp-pylsp-plugins-flake8-ignore "E128,E261,E265,E302,E401,E501,E713,E741"
        lsp-pylsp-plugins-pydocstyle-enabled nil
        lsp-pylsp-plugins-mccabe-threshold 40
        lsp-tailwindcss-add-on-mode t)

  ;; Use an alternative typescript lsp, install via npm
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
                     (lambda ()
                       `("node" ,(expand-file-name "~/.nvm/versions/node/v20.12.2/bin/vtsls") "--stdio")))
    :priority -1
    :major-modes '(typescript-mode)
    :server-id 'vtsls)))

(add-hook 'typescript-mode-hook
          (lambda ()
            (setq-local lsp-enabled-clients '(eslint tailwindcss vtsls))
            (lsp-deferred)))

;; set flycheck cpp standard
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")))

(setq company-lsp-enable-snippet t)
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(yas-global-mode 1)

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

;; compilation buffer: autosave and stop at the first error and skip warnings
(setq compilation-scroll-output 'next-error
      compilation-skip-threshold 2
      ;; do not save before compilation
      ;compilation-save-buffers-predicate 'ignore
      )

(set-default 'truncate-lines nil)

;; MAGIT
(after! magit
  (setq git-commit-summary-max-length 120))

;; Corfu (completion)
(after! corfu
  (setq corfu-auto t
        corfu-cycle t
        corfu-quit-no-match 'separator
        corfu-preselect 'prompt
        corfu-preview-current nil))

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

(use-package! elysium
  :defer t
  :custom
  (elysium-window-size 0.33)       ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical) ; Can be customized to horizontal)
  ; enable smerge-mode explicitely
  ;(add-hook! 'elysium-apply-changes-hook #'smerge-mode)
  :hook (elysium-apply-changes . smerge-mode))

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
