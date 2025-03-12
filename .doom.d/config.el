(message "*** General")

(load! "+functions")

(setq user-full-name "Andreas Pohl"
      user-mail-address "pohl@e47.org")

;; Maximize emacs w/o setting a frame property to keep normal macOS window management working
(add-hook 'doom-init-ui-hook
  (lambda ()
    (set-frame-position (selected-frame) 0 0)
    (set-frame-size (selected-frame) (- (display-pixel-width) 16) (display-pixel-height) t)))

;; Restore last session automatically
(add-hook! 'window-setup-hook #'my-quickload-session)

(message "*** General / General behavior")

;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Disable the quit question
(setq confirm-kill-emacs nil)

;; Make sure we get asked to accept non-safe local variables from .dir-locals.el files
(setq enable-local-variables :all)

;; "ctrl - left click" buffer menu: increase number of items shown.
;; Set max length of this list. default 20. see next.
(setq mouse-buffer-menu-maxlen 30)

;; set # buffer in a mode before grouping begins. takes precedence over previous
;; set to 1 to always group by mode. default 4
(setq mouse-buffer-menu-mode-mult 1)

;; Do not truncate lines but wrap them into the next line
(setq-default truncate-lines nil)

;; Desired line length
(setq-default fill-column 120)

;; File name in the mode line
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

(message "*** General / Org")

(setq org-directory "~/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/"
      org-support-shift-select t
      org-replace-disputed-keys t
      org-startup-indented t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

(message "*** General / Spellchecker")

(use-package! jinx
  :hook ((org-mode . jinx-mode)
         (prog-mode . jinx-mode))
  :config
  (setq jinx-languages "en_US de_DE_frami"
        jinx-delay 1.0)
  ; no spell checking in strings
  (add-to-list 'jinx-exclude-faces '(prog-mode font-lock-string-face font-lock-comment-face)))

(after! vertico-multiform ;; if using vertico
  (add-to-list 'vertico-multiform-categories
               '(jinx (vertico-grid-annotate . 25))))

(message "*** Key Bindings")

(undefine-key! "C-z" "s-w" "s-+" "s--")
(setq doom-localleader-alt-key "C-z")

(map!
 ;; treemacs
 "s-1" #'treemacs
 "s-2" #'treemacs-tag-follow-mode
 "s-3" #'treemacs-project-follow-mode
 ;; navigation
 "C-x p" #'my-previous-window
 "C-x n" #'other-window
 "M-<left>" #'outline-hide-subtree
 "M-<right>" #'outline-show-children
 "M-<up>" #'outline-hide-other
 "M-<down>" #'outline-show-all
 "<home>" #'beginning-of-line
 "<end>" #'end-of-line
 "C-x r e" #'replace-regexp
 "C-x c p" #'my-match-paren
 "C-c x" #'dabbrev-expand
 "C-c b" #'revert-buffer
 "C-c u" #'upcase-region
 "C-c d" #'downcase-region
 "C-c f" #'find-file-at-point
 "C-c C-a" #'auto-fill-mode
 "C-c j" #'set-justification-left
 "M-g" #'goto-line
 "C-x C-y" #'my-save-and-killbuf
 ;; code navigation
 "s-." #'xref-find-definitions
 "s-," #'xref-go-back
 ;; buffers and font
 "<s-wheel-down>" #'enlarge-window-horizontally
 "<s-wheel-up>" #'shrink-window-horizontally
 "s-*" #'doom/increase-font-size
 "s-_" #'doom/decrease-font-size
 ;; gptel/elysium
 (:leader :prefix ("C-s" . "LLM")
          (:prefix ("e" . "elysium")
                   "e" #'elysium-query
                   "w" #'elysium-toggle-window
                   "a" #'elysium-apply-code-changes
                   "d" #'elysium-discard-all-suggested-changes)
          "w" #'gptel
          (:prefix ("a" . "add")
                   "r" #'gptel-add
                   "f" #'gptel-add-file))
 ;; lsp-bridge
 ;(:leader :prefix ("c" . "code")
 ;         :desc "LSP Code actions"      "a"   #'lsp-bridge-code-action
 ;         :desc "LSP Rename"            "r"   #'lsp-bridge-rename
 ;         :desc "Find References"       "i"   #'lsp-bridge-find-references
 ;         :desc "Find Definition"       "j"   #'lsp-bridge-find-def
 ;         :desc "Find Implementation"   "J"   #'lsp-bridge-find-impl)
 ;; miscellaneous
 "M-s <up>" #'comint-previous-input
 "M-s <down>" #'comint-next-input
 "C-c w Q" #'my-quickload-session
 ;; mode specific
 :map (c++-mode-map c-mode-map cmake-mode-map objc-mode-map)
 "C-c RET" #'recompile
 ;:map (c++-mode-map c-mode-map typescript-mode-map js-mode-map java-mode-map)
 ;"s-." #'lsp-bridge-peek
 ;"s-," #'lsp-bridge-peek-jump-back
 ;:map lsp-bridge-peek-keymap
 ;"s-." #'lsp-bridge-peek-jump
 ;"RET" #'lsp-bridge-peek-jump
 ;"<up>" #'lsp-bridge-peek-list-prev-line
 ;"<down>" #'lsp-bridge-peek-list-next-line
 :map gptel-mode-map
 "C-c RET" #'gptel-menu
 "C-<return>" #'gptel-send
 "C-<up>" #'gptel-beginning-of-response
 "C-<down>" #'gptel-end-of-response
 :map vterm-mode-map
 "C-c C-c" #'vterm-send-C-c
 :map mu4e-headers-mode-map
 "." #'mu4e-view-raw-message
 "<up>" #'mu4e-headers-prev
 "<down>" #'mu4e-headers-next
 "v" #'mu4e-views-mu4e-select-view-msg-method
 "M-n" #'mu4e-views-cursor-msg-view-window-down
 "M-p" #'mu4e-views-cursor-msg-view-window-up
 "f" #'mu4e-views-toggle-auto-view-selected-message
 "i" #'mu4e-views-mu4e-view-as-nonblocked-html
 :map org-msg-edit-mode-map
 "C-c C-c" #'my-org-msg-ctrl-c-ctrl-c
 )

(after! treemacs
  (treemacs-define-RET-action 'file-node-closed #'treemacs-visit-node-ace)
  (treemacs-define-RET-action 'file-node-open #'treemacs-visit-node-ace)
  (define-key treemacs-mode-map [s-mouse-1] #'treemacs-visit-node-ace))

;; Switch between header and implementation, replace projectile version as this one here works outside of projects
(add-hook 'c-initialization-hook
          (lambda () (define-key c-mode-base-map (kbd "C-c p a") 'ff-get-other-file)))

;; buffer selection cia <cmd>+<left click> in c++
(global-set-key [s-mouse-1] 'mouse-buffer-menu)

;; use more convinient smerge key bindings
(setq smerge-command-prefix "\C-cm")

; redo
(after! undo-fu
  (map! :map undo-fu-mode-map "C-?" #'undo-fu-only-redo))

(message "*** Looks")

(setq doom-theme 'doom-city-lights)

(defvar my-fixed-font "Iosevka Comfy")
(defvar my-variable-font "Roboto")

(setq doom-font
      (font-spec :family my-fixed-font :size 13)
      doom-variable-pitch-font
      (font-spec :family my-variable-font :size 13))

;; zoom in/out steps
(setq doom-font-increment 1)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Always fixed font even in variable-pitch-mode
(set-face-attribute 'line-number nil :font my-fixed-font)
(set-face-attribute 'line-number-current-line nil :font my-fixed-font)

;; Set the project name as frame title (window name in macOS)
(setq frame-title-format '("" "%b" (:eval
                                    (let ((project-name (projectile-project-name)))
                                      (unless (string= "-" project-name)
                                        (format " in [%s]" project-name))))))

(add-hook 'doom-init-ui-hook
  (lambda ()
    ;; Enable/disable toolbar mode to set the proper (minimal) titlebar height (macOS)
    (tool-bar-mode 1)
    (tool-bar-mode 0)))

(after! treemacs
  (setq treemacs-width 45)
  (treemacs-follow-mode 1)
  (treemacs-project-follow-mode 1)
  (set-face-attribute 'treemacs-root-face nil :height 1.0)
  ;; treemacs png/svg special icons don't look great, so we patch the icon set
  (add-hook 'treemacs-mode-hook 'my-update-treemacs-icons))

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

(message "*** Coding / General")

;; Compilation buffer: stop at the first error and skip warnings
(setq compilation-scroll-output 'next-error
      compilation-skip-threshold 2)

(defvar my-cpp-other-file-alist
  '(("\\.cpp\\'" (".h" ".hpp" ".ipp"))
    ("\\.ipp\\'" (".hpp" ".cpp"))
    ("\\.hpp\\'" (".ipp" ".cpp"))
    ("\\.cxx\\'" (".hxx" ".ixx"))
    ("\\.ixx\\'" (".cxx" ".hxx"))
    ("\\.hxx\\'" (".ixx" ".cxx"))
    ("\\.cc\\'" (".h" ".hh"))
    ("\\.mm\\'" (".h"))
    ("\\.m\\'" (".h"))
    ("\\.c\\'" (".h"))
    ("\\.h\\'" (".cpp" ".cc" ".cxx" ".c" ".mm"))))

(setq-default ff-other-file-alist 'my-cpp-other-file-alist)

(add-hook 'c-mode-common-hook 'my-clang-format-indent)
(add-hook 'c++-mode-hook 'my-clang-format-indent)

(setq projectile-completion-system 'default)

(message "*** Coding / Git")

;; Make the git summary line longer
(after! magit
  (setq git-commit-summary-max-length 120))

(message "*** Coding / LSP - lsp-mode")
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
        ;lsp-tailwindcss-add-on-mode t
        ;; Java setup
        lsp-java-server-install-dir "/Users/andreas/tools/jdtls"
        lsp-java-jdt-ls-prefer-native-command t
        lsp-java-configuration-update-build-configuration "interactive")

  ;; Use an alternative typescript lsp, install via npm
  ;; npm install -g @vtsls/language-server
  ;(lsp-register-client
  ; (make-lsp-client
  ;  :new-connection (lsp-stdio-connection
  ;                   (lambda ()
  ;                     `("node" ,(expand-file-name "~/.nvm/versions/node/v20.12.2/bin/vtsls") "--stdio")))
  ;  :priority -1
  ;  :major-modes '(typescript-mode)
  ;  :server-id 'vtsls))
  )

;; Java LSP configuration is now included directly in the lsp-mode config block

(add-hook 'typescript-mode-hook
          (lambda ()
            ;(setq-local lsp-enabled-clients '(eslint tailwindcss ts-ls))
            (setq-local lsp-enabled-clients '(ts-ls eslint))
            (lsp-deferred)))

;; Set flycheck cpp standard
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")))

(message "*** Coding / Debugging")

(use-package! dap-mode
  :ensure t
  :after lsp-mode
  :config
  (require 'dap-launch)
  (require 'dap-java)
  (require 'dap-lldb)

  (setq dap-lldb-debug-program '("/Applications/Xcode.app/Contents/Developer/usr/bin/lldb-dap"))

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-ui-controls-mode 1)
  (dap-tooltip-mode 1)
  (dap-auto-configure-mode 1)

  ;(require 'dap-codelldb)
  ;(dap-codelldb-setup)

  ;; Register a default debug template for C++ projects
  ;;(dap-register-debug-template
  ;;  "C++ LLDB::Run"
  ;;  (list :type "lldb"
  ;;        :request "launch"
  ;;        :name "C++ LLDB::Run"
  ;;        :program "${workspaceFolder}/"
  ;;        :cwd nil))
  (dap-register-debug-template
  "lldb-dap ms"
  (list :type "lldb"
        :request "launch"
        :name "lldb-dap ms"
        :program "${workspaceFolder}/build-dev/bin/sdna-mediaserver"
        :args nil
        :cwd nil
        :stopOnEntry t
        :preLaunchTask "lldb-dap"
        :environment nil
        :debugger-args nil))
  (dap-register-debug-template
   "C++ LLDB Debug MS"
   (list :type "lldb-vscode"
         :request "launch"
         :name "C++ LLDB Debug MS"
         :program "${workspaceFolder}/build-dev/bin/sdna-mediaserver"
         :args '()
         :cwd "${workspaceFolder}"
         :stopAtEntry nil
         :externalConsole nil))
  )

(message "*** Coding / Mode Mapping")

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

(message "*** Coding / Tree-Sitter")

(add-to-list 'major-mode-remap-alist '(js-ts-mode . js-mode))
(add-to-list 'major-mode-remap-alist '(typescript-ts-mode . typescript-mode))
(add-to-list 'major-mode-remap-alist '(tsx-ts-mode . typescript-mode))

(message "*** Coding / Templates")

;; Set up default file templates based on the project
(set-file-template! "\\.hpp$" :trigger "__hpp" :mode 'c++-mode)
(set-file-template! "\\.cpp$" :trigger "__cpp" :mode 'c++-mode)
(set-file-template! "syncdna.*\\.hpp$" :trigger "sdna_hpp" :mode 'c++-mode)
(set-file-template! "syncdna.*\\.cpp$" :trigger "sdna_cpp" :mode 'c++-mode)
(set-file-template! "AudioGridder.*\\.hpp$" :trigger "ag_hpp" :mode 'c++-mode)
(set-file-template! "AudioGridder.*\\.cpp$" :trigger "ag_cpp" :mode 'c++-mode)

(add-hook 'find-file-hook
          (lambda ()
            (when (and (= (buffer-size) 0))
              (+file-templates/apply))))

(message "*** Coding / Terminal")

(advice-add 'mwheel-scroll :after #'my-scroll-mouse-handler)

(message "*** Coding / Compilation Buffer")

(add-hook 'compilation-start-hook 'my-compilation-started)
(add-hook 'compilation-finish-functions 'my-hide-compile-buffer-if-successful)

(message "*** LLM")

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
  (gptel-model 'claude-3-7-sonnet-20250219)
  :config
  (setq gptel-default-mode 'org-mode)

  ;; OpenAI
  (setq! gptel-api-key (my-read-file "~/.gptel/chatgpt.key"))

  ;; Google
  (defun gptel-gemini-api-key ()
    (my-read-file "~/.gptel/gemini.key"))
  (gptel-make-gemini "Gemini" :stream t
                     :key #'gptel-gemini-api-key)

  ;; Anthropic (default)
  (defun gptel-claude-api-key ()
    (my-read-file "~/.gptel/claude.key"))
  (setq gptel-backend
        (gptel-make-anthropic "Claude" :stream t
                              :key #'gptel-claude-api-key)))
