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

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;(setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;      doom-variable-pitch-font (font-spec :family "sans" :size 13))
;(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12)
;      ;;doom-variable-pitch-font (font-spec :family "ETBembo" :size 18)
;      doom-variable-pitch-font (font-spec :family "Alegreya" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;(setq doom-theme 'doom-one)
(setq doom-theme 'doom-city-lights)

;(plist-put (alist-get "Reload last session" +doom-dashboard-menu-sections nil nil 'equal)
;           :action doom/quickload-session)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; More colors in treesitter
(setq treesit-font-lock-level 4)

;; Treemacs tweaks
(after! treemacs
  (setq treemacs-width 45)
  (treemacs-follow-mode 1)
  ;; The treemacs png icons are not rendered properly, so we replace them with default icons from the font
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("src-closed" "test-closed" "tmp-closed" "temp-closed" "build-closed"
                                     "bin-closed" "git-closed" "github-closed" "public-closed" "private-closed"
                                     "screenshot-closed" "icons-closed" "readme-closed" "docs-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("src-open" "test-open" "tmp-open" "temp-open" "build-open" "bin-open"
                                     "git-open" "github-open" "public-open" "private-open" "screenshot-open"
                                     "icons-open" "readme-open" "docs-open"))
  (treemacs-create-icon :icon (propertize " 	󱆃	" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("zshrc" "bash" "bash_profile" "bash_login" "bash_aliases" "profile"))
  ;; Now replace all remaining icons that are images with the fallback file icon
  (let ((icons (treemacs-theme->gui-icons treemacs--current-theme))
        (def-icon (gethash 'fallback (treemacs-theme->gui-icons treemacs--current-theme))))
    (maphash
     (lambda (k v)
       (when (imagep (get-text-property 0 'display v))
         (puthash k def-icon icons)
         ))
     icons)
   ))

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
(setq frame-title-format
    '(""
      "%b"
      (:eval
       (let ((project-name (projectile-project-name)))
         (unless (string= "-" project-name)
           (format " in [%s]" project-name))))))

;; Hide commands in M-x which do not apply to the current mode.
(setq read-extended-command-predicate #'command-completion-default-include-p)

;; Restore last session automatically
(add-hook! 'window-setup-hook #'my-quickload-session)


;;
;; CODING - LSP, CODE COMPLETION etc
;;

;; an alternative to the standard typescript lsp
;; npm install -g @vtsls/language-server
(use-package! lsp-mode
  :config
  (setq lsp-disabled-clients '(ccls))
  (setq lsp-idle-delay 0.9)
  (setq lsp-restart 'auto-restart)
  (setq lsp-ui-doc-enable nil)
  ;(setq lsp-log-io t)
  ;(setq lsp-file-watch-threshold nil)
  ;(setq lsp-enable-file-watchers nil)
  ;; disable flycheck
  ;(setq lsp-diagnostics-provider :none)
  ;; Use xcode's clangd
  ;(setq lsp-clients-clangd-executable
  ;      "/Library/Developer/CommandLineTools/usr/bin/clangd")
  ;(setq lsp-clients-clangd-executable
  ;      "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/clangd")
  (setq lsp-clients-clangd-args '("--log=error"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--pretty"))
  (setq lsp-pylsp-plugins-flake8-ignore "E128,E261,E265,E302,E401,E501,E713,E741")
  (setq lsp-pylsp-plugins-pydocstyle-enabled nil)
  (setq lsp-pylsp-plugins-mccabe-threshold 40)
  (setq lsp-tailwindcss-add-on-mode t)

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
            (lsp-deferred)
            ))

;; set flycheck cpp standard
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")
            ))

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
(setq compilation-scroll-output 'next-error)
(setq compilation-skip-threshold 2)
;; do not save before compilation
;(setq compilation-save-buffers-predicate 'ignore)

(set-default 'truncate-lines nil)

;; MAGIT
(after! magit
  (setq git-commit-summary-max-length 120))

;; Corfu (completion)
(after! corfu
  (setq corfu-auto t)
  (setq corfu-cycle t)
  (setq corfu-quit-no-match 'separator)
  (setq corfu-preselect 'prompt)
  (setq corfu-preview-current nil))

;; DAP-MODE
;(setq dap-auto-configure-mode t)
;(setq dap-lldb-debug-program "/opt/homebrew/opt/llvm/bin/lldb-dap")
;(setq dap-gdb-lldb-path "/usr/bin/lldb")
;(require 'dap-cpptools)
;; Enabling only some features
;(setq dap-auto-configure-features '(sessions locals controls tooltip))

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
          :cwd nil))
  )

(use-package! elysium
  :custom
  (elysium-window-size 0.33)       ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical) ; Can be customized to horizontal)
  :config
  ; enable smerge-mode explicitely
  (add-hook 'elysium-apply-changes-hook #'smerge-mode))

(use-package! gptel
  :custom
  (gptel-model 'claude-3-5-sonnet-20241022)
  :config
  (defun read-file-contents (file-path)
    "Read the contents of FILE-PATH and return it as a string."
    (with-temp-buffer
      (insert-file-contents file-path)
      (buffer-string)))
  (defun gptel-api-key ()
    (read-file-contents "~/.claude.key"))
  (setq gptel-backend
        (gptel-make-anthropic "Claude"
                              :stream t
                              :key #'gptel-api-key)))
