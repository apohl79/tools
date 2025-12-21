(message "*** General")

(load! "+functions")

(setq user-full-name "Andreas Pohl"
      user-mail-address "pohl@e47.org")

;; Restore frame geometry and font size from last session
(add-hook 'doom-init-ui-hook #'my/restore-frame-geometry)

;; Save frame geometry and font size on exit
(add-hook 'kill-emacs-hook #'my/save-frame-geometry)

;; Restore last session automatically
(add-hook 'doom-after-init-hook #'my/quickload-session)

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

(message "*** General / Dashboard")

;; Add the projects widget to the dashboard (without the shortmenu)
;; Widget function is defined in +functions.el
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        my/doom-dashboard-widget-projects
        doom-dashboard-widget-loaded))

;; Track last workspace switch time for sorting (function defined in +functions.el)
(add-hook 'persp-activated-functions #'my/update-workspace-switch-time)

;; Disable line numbers in dashboard - multiple approaches for reliability
(add-hook '+doom-dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

(add-hook 'doom-dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

(message "*** General / Org")

(setq org-directory "~/ownCloud/org"
      org-support-shift-select t
      org-replace-disputed-keys t
      org-startup-indented t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Unbind the C-x n prefix to allow using it for other-window
(map! :after org
      :map org-mode-map
      "C-x n" nil)

(after! markdown-mode
  ;; Unbind the C-x n prefix to allow using it for other-window
  (map! :map markdown-mode-map
        "C-x n" nil)

  ;; Make URLs clickable with mouse and C-c RET
  (add-hook! 'markdown-mode-hook
    #'goto-address-mode
    #'visual-line-mode))

(use-package! pdf-tools
  :config
  (pdf-tools-install))

;; Properly detect and handle CRLF files
(setq inhibit-eol-conversion t)

;; More flexible EOL handling
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(message "*** Key Bindings")

(undefine-key! "C-z" "s-w" "s-+" "s--")
(setq doom-localleader-alt-key "C-z")

(map!
 ;; treemacs
 "s-1" #'treemacs
 "s-2" #'treemacs-tag-follow-mode
 "s-3" #'treemacs-project-follow-mode

 ;; navigation
 "C-x p" #'my/previous-window
 "C-x n" #'other-window
 "M-<left>" #'outline-hide-subtree
 "M-<right>" #'outline-show-children
 "M-<up>" #'outline-hide-other
 "M-<down>" #'outline-show-all
 "<home>" #'beginning-of-line
 "<end>" #'end-of-line
 "C-x r e" #'replace-regexp
 "C-x c p" #'my/match-paren
 "C-c x" #'dabbrev-expand
 "C-c b" #'revert-buffer
 "C-c u" #'upcase-region
 "C-c d" #'downcase-region
 "C-c f" #'find-file-at-point
 "C-c C-a" #'auto-fill-mode
 "C-c j" #'set-justification-left
 "M-g" #'goto-line
 "C-x C-y" #'my/save-and-killbuf

 ;; code navigation
 "s-." #'xref-find-definitions
 "s-," #'xref-go-back

 ;; buffers and font
 "<s-wheel-down>" #'enlarge-window-horizontally
 "<s-wheel-up>" #'shrink-window-horizontally
 "s-*" #'doom/increase-font-size
 "s-_" #'doom/decrease-font-size

;; claude-code
;;(:prefix ("C-s-x" . "Claude")
;;         "c" #'claude-code-ide
;;         "r" #'claude-code-ide-resume
;;         "k" #'claude-code-ide-stop
;;         "RET" #'claude-code-ide-insert-newline
;;         "ESC" #'claude-code-ide-send-escape)

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

 ;; kubernetes
 "C-c k" #'kubernetes-overview

 ;; miscellaneous
 "M-s <up>" #'comint-previous-input
 "M-s <down>" #'comint-next-input
 "C-c w Q" #'my/quickload-session

 ;; mode specific
 :map (prog-mode-map)
 "C-c RET" #'recompile
 "TAB" #'my/indent-or-tab
 :map (c-ts-base-mode-map)
 "RET" #'my/clang-format-newline-and-indent
 :desc "Copy Impl Body to Clipboard" "C-c c g" #'my/generate-cpp-implementation
 :map (protobuf-mode-map)
 "C-c ;" #'+company/dabbrev
 ;:map (typescript-ts-base-mode-map)
 ;"TAB" #'treesit-indent
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
 "C-c C-c" #'my/org-msg-ctrl-c-ctrl-c
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

(message "*** Email")

(use-package! mu4e
  ;; Fix the setup of org-msg for mu4e
  :init (add-hook 'org-msg-mode-hook
                  (lambda ()
                    (org-msg-mode-mu4e)
                    (org-msg-edit-mode-mu4e)
                    ;; this fixes the problem of not closing the edit buffer properly
                    (add-hook 'message-sent-hook
                              (lambda ()
                                (my/message-kill-buffer-no-query)
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
  (mu4e-views-mu4e-use-view-msg-method "gnus")) ;; select the default

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "arc-cli"
      browse-url-generic-args '("new-little-arc"))

(message "*** Looks")

(setq doom-theme 'doom-city-lights)

;(defvar my/fixed-font "Iosevka Comfy")
(defvar my/fixed-font "IosevkaTerm NFM Medium")

(defvar my/unicode-font "JuliaMono")
;(defvar my/unicode-font "IosevkaTerm NFM Medium")

(defvar my/variable-font "Roboto")

(setq doom-font
      (font-spec :family my/fixed-font :size 13)
      doom-variable-pitch-font
      (font-spec :family my/variable-font :size 13))

;; zoom in/out steps
(setq doom-font-increment 1)

;; Set line spacing to reduce gaps between vertical bars
(setq line-spacing 0.1)

;; IMPORTANT: Set this to nil so custom fontset is used
(setq use-default-font-for-symbols nil)

;; Define function to configure fontsets
(defun my/configure-fontsets ()
  "Configure fontsets for unicode and symbol characters."

  (set-fontset-font t 'symbol nil)

  ;; General unicode/symbol setup - use unicode font with smaller size for icons
  (set-fontset-font t 'unicode (font-spec :family my/unicode-font :size 10.5) nil 'prepend)
  (set-fontset-font t 'symbol (font-spec :family my/unicode-font :size 10.5) nil 'prepend)

  ;; Box-drawing and geometric shapes to align vterm buffer width properly
  (set-fontset-font t '(#x2500 . #x257F) (font-spec :family my/fixed-font) nil 'prepend)
  (set-fontset-font t '(#x2580 . #x259F) (font-spec :family my/fixed-font) nil 'prepend)
  (set-fontset-font t '(#x25A0 . #x25FF) (font-spec :family my/fixed-font) nil 'prepend)

  ;; Fix non-breaking space underlines
  (set-face-attribute 'nobreak-space nil :underline nil)
)

;; Apply after Doom sets fonts and on zoom
(add-hook 'after-setting-font-hook #'my/configure-fontsets)
(add-hook 'doom-init-ui-hook #'my/configure-fontsets)
;; Also apply when loading vterm buffers
(add-hook 'vterm-mode-hook #'my/configure-fontsets)
(add-hook 'eat-mode-hook #'my/configure-fontsets)

;; Replace specific Claude Code Unicode symbols with ASCII in vterm/eat buffers
;; ⠉ ⠒ ⠤
(defun my/replace-unicode-spinners ()
  "Set buffer-local display table to replace Unicode spinners with ASCII in vterm."
  (let ((table (or buffer-display-table (make-display-table))))
    ;; · - U+00B7 (Middle Dot)
    (aset table #x00B7 (vector ?⠉))
    ;; ✢ - U+2722 (Four Teardrop-Spoked Asterisk)
    (aset table #x2722 (vector ?⠉))
    ;; ✳ - U+2733 (Eight Spoked Asterisk)
    (aset table #x2733 (vector ?⠒))
    ;; ✶ - U+2736 (Six Pointed Black Star)
    (aset table #x2736 (vector ?⠒))
    ;; ✻ - U+273B (Teardrop-Spoked Asterisk)
    (aset table #x273B (vector ?⠤))
    ;; ✽ - U+273D (Heavy Teardrop-Spoked Asterisk)
    (aset table #x273D (vector ?⠤))
    (setq buffer-display-table table)))

(add-hook 'vterm-mode-hook #'my/replace-unicode-spinners)
(add-hook 'eat-mode-hook #'my/replace-unicode-spinners)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Always fixed font even in variable-pitch-mode
(set-face-attribute 'line-number nil :font my/fixed-font)
(set-face-attribute 'line-number-current-line nil :font my/fixed-font)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))

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
  (add-hook 'treemacs-mode-hook 'my/update-treemacs-icons))

;; Dim inactive buffers to highlight the active one
(use-package! dimmer
  :config
  (dimmer-mode t)
  ;; Adjust dimming percentage (0.0-1.0, lower = darker)
  (setq dimmer-fraction 0.20))

(after! org-mode
  (setq org-support-shift-select t
        org-replace-disputed-keys t))

(after! org
  ;; Add TypeScript to org-babel languages
  (add-to-list 'org-babel-load-languages '(typescript . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

  ;; Enable syntax highlighting for TypeScript blocks
  (add-to-list 'org-src-lang-modes '("typescript" . typescript-ts))

  (defun org-add-color-keywords ()
    (font-lock-add-keywords
     nil
     '(("{\\([a-zA-Z#0-9]+\\):\\([^}]+\\)}"
        (0 (let ((color (match-string 1))
                 (start (match-beginning 0))
                 (color-end (match-end 1))
                 (text-start (match-beginning 2))
                 (text-end (match-end 2))
                 (end (match-end 0)))
             ;; Hide the opening bracket and color name
             (put-text-property start (1+ color-end) 'invisible t)
             ;; Color the text
             (add-face-text-property text-start text-end `(:foreground ,color))
             ;; Hide the closing bracket
             (put-text-property text-end end 'invisible t)
             nil))))))

  (add-hook 'org-mode-hook 'org-add-color-keywords))

(use-package! org-modern
  :after org
  :hook ((org-mode . global-org-modern-mode)
         ;(org-mode . (lambda ()
         ;              ;; increase line spacing a little
         ;              (setq-local default-text-properties '(line-spacing 0.1 line-height 1.1))))
         )
  :config
  (setq org-modern-star 'replace
        org-modern-label-border 0.3
        org-modern-table-vertical 1
        org-modern-replace-stars "￭￭￭￭￭"
        org-modern-todo-faces (quote (("WAIT" :inherit org-modern-todo :height 1.2 :foreground "goldenrod")
                                      ("HOLD" :inherit org-modern-todo :height 1.2 :foreground "indian red")
                                      ("DONE" :inherit org-modern-todo :height 1.2 :inverse-video nil
                                       :foreground "white" :distant-foreground "white" :background "grey25"))))

  ;; Make the document title a bit bigger
  ;(set-face-attribute 'org-document-title nil :font my/variable-font :weight 'bold :height 1.3 :underline t)
  (set-face-attribute 'org-document-title nil :font my/fixed-font :weight 'bold :height 1.2 :underline t)

  ;; Set the table color
  (set-face-attribute 'org-table nil :foreground "#D4AF37")

  ;; Resize headings
  (dolist (face '((org-level-1 . 1.1)
                  (org-level-2 . 1.0)
                  (org-level-3 . 1.0)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
    ;(set-face-attribute (car face) nil :font my/variable-font :height (cdr face))
    (set-face-attribute (car face) nil :font my/fixed-font :height (cdr face))))

(after! org-modern-faces
  (set-face-attribute 'org-modern-symbol nil :family my/fixed-font))

;(use-package! mixed-pitch
;  :after org
;  :hook (org-mode . mixed-pitch-mode))

(use-package! valign
  :hook (markdown-mode . valign-mode))

(use-package! pgmacs
  :config
  (set-face-attribute 'pgmacs-table-data nil :foreground "gray")
  (set-face-attribute 'pgmacs-column-foreign-key nil :foreground "orange")
  (setq pgmacs-row-colors '("#1D252C" "#181E24")
        pgmacs-deleted-color "#B93448")
  )

(message "*** Coding / General")

;; Compilation buffer: stop at the first error and skip warnings
(setq compilation-scroll-output 'next-error
      compilation-skip-threshold 2)

(defvar my/cpp-other-file-alist
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

(setq-default ff-other-file-alist 'my/cpp-other-file-alist)

(setq projectile-completion-system 'default)

(message "*** Coding / Git")

;; Make the git summary line longer
(after! magit
  (setq git-commit-summary-max-length 120))

(message "*** Coding / LSP - lsp-mode")
(use-package! lsp-mode
  :defer t
  :hook ((c++-ts-mode . lsp-deferred)
         (java-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred))
  :config
  (setq lsp-disabled-clients '(ccls)
        lsp-idle-delay 0.9
        lsp-enable-file-watchers nil  ; Disable file watching to avoid prompts
        lsp-file-watch-threshold 2000
        lsp-restart 'auto-restart
        lsp-ui-doc-enable nil
        lsp-enable-indentation nil
        lsp-modeline-code-actions-enable t
        lsp-log-io t  ; Enable LSP communication logging
        ;; Use xcode's clangd
        lsp-clients-clangd-executable "/opt/homebrew/Cellar/llvm@19/19.1.7/bin/clangd"
        lsp-clients-clangd-args '("--log=error"
                                  "--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=never"
                                  "--pretty"
                                  "--function-arg-placeholders")
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

(add-hook 'typescript-ts-mode-hook
          (lambda ()
            ;(setq-local lsp-enabled-clients '(eslint tailwindcss ts-ls))
            (setq-local lsp-enabled-clients '(ts-ls eslint))
            (lsp-deferred)))

(add-hook 'tsx-ts-mode-hook
          (lambda ()
            ;(setq-local lsp-enabled-clients '(eslint tailwindcss ts-ls))
            (setq-local lsp-enabled-clients '(ts-ls eslint))
            (lsp-deferred)))

;; Configure C/C++ tree-sitter indentation to match clang-format
(setq c-ts-mode-indent-offset 4)
(setq c++-ts-mode-indent-offset 4)

;; Set flycheck cpp standard and fix indentation rules
(add-hook 'c++-ts-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++17")
            (setq-local c-ts-mode-indent-offset 4)
            (setq-local tab-width 4)))

(message "*** Coding / Debugging")

(use-package! dap-mode
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
      (append '(("\\.app$"                  . c++-ts-mode)
                ("\\.bat$"                  . rexx-mode)        ; to edit batchfiles
                ("\\.bib$"                  . bibtex-mode)      ;
                ("\\.btm$"                  . rexx-mode)
                ("\\.C$"                    . c++-ts-mode)
                ("\\.i$"                    . c++-ts-mode)         ; SWIG: use c++-mode
                ("\\.cc$"                   . c++-ts-mode)
                ("\\.cpp$"                  . c++-ts-mode)
                ("\\.H$"                    . c++-ts-mode)
                ("\\.h$"                    . c++-ts-mode)
                ("\\.hi$"                   . c-ts-mode)
                ("\\.hpp$"                  . c++-ts-mode)
                ("\\.idl$"                  . c++-ts-mode)
                ("\\.c$"                    . c-ts-mode)           ; to edit C code
                ("\\.sqc$"                  . c-ts-mode)           ; NON-Preprocessed C with DB/2 SQL
                ("\\.rc$"                   . c-ts-mode)           ; Files from rc are also smth like c
                ("\\.rci$"                  . c-ts-mode)           ; Files from rc are also smth like c
                ("\\.rcx$"                  . c-ts-mode)           ; Files from rc are also smth like c
                ("\\.cmd$"                  . rexx-mode)        ; to edit REXX-Skripte
                ("\\.c?ps$"                 . postscript-mode)  ; Fuer postscript-files
                ("\\.tex$"                  . latex-mode)       ; tbd
                ("\\.sm$"                   . latex-mode)       ;
                ("\\.sty$"                  . latex-mode)       ;
                ("\\.mak$"                  . makefile-mode)
                ("makefile$"                . makefile-mode)
                ("\\.java$"                 . java-ts-mode)
                ("\\.jav$"                  . java-ts-mode)
                ("\\.py$"                   . python-mode)
                ("\\.xh$"                   . c++-ts-mode)
                ("\\.xih$"                  . c++-ts-mode)
                ("\\.in$"                   . m4-mode)
                ("\\.\\([pP][Llm]\\|al\\)$" . cperl-mode)
                ("\\.pod$"                  . cperl-mode)
                ("\\.puml$"                 . plantuml-mode)
                ("\\.ino$"                  . c++-ts-mode)
                ("\\.ts$"                   . typescript-ts-mode)
                ("\\.tsx$"                  . jtsx-tsx-mode)
                ("\\.tf$"                   . terraform-mode)
                ("\\.hcl$"                  . terraform-mode)
                ) auto-mode-alist))

(use-package! jtsx
  :custom
  (jtsx-enable-all-syntax-highlighting-features t))

(message "*** Coding / Tree-Sitter")

;;(add-to-list 'major-mode-remap-alist '(js-ts-mode . js-mode))
;;(add-to-list 'major-mode-remap-alist '(typescript-ts-mode . typescript-mode))
;;(add-to-list 'major-mode-remap-alist '(tsx-ts-mode . typescript-mode))

(use-package! treesit
  :config
  (setq treesit-font-lock-level 4
        treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (proto "https://github.com/Clement-Jean/tree-sitter-proto")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")))

  (defun my/treesit-install-all-grammars ()
    "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
    (interactive)
    (dolist (grammar treesit-language-source-alist)
      (let ((lang (car grammar)))
        (message "Installing tree-sitter grammar for %s..." lang)
        (treesit-install-language-grammar lang)))
    (message "All tree-sitter grammars installed!"))

  ;; Map major modes to their tree-sitter equivalents
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
                                        ;(protobuf-mode . proto-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (javascript-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (sh-mode . bash-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (python-mode . python-ts-mode))))

(use-package! clang-format
  :init
  ;; update the indent style to disable namespace indention with treesit-indent
  (defun my/c-ts-indent-style-no-namespace()
    "Custom indent style based on Google style with 4-space indentation."
    ;; Start with k&r style which is closer to Google style than gnu
    (let ((base-style (alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))
      `(;; Namespace members should not be indented
        ((n-p-gp nil nil "namespace_definition") grand-parent 0)
        ;; Override k&r to use 4 spaces instead of default offset
        ((parent-is "compound_statement") standalone-parent 4)
        ((parent-is "if_statement") standalone-parent 4)
        ((parent-is "else_clause") standalone-parent 4)
        ((parent-is "do_statement") standalone-parent 4)
        ((parent-is "for_statement") standalone-parent 4)
        ((parent-is "while_statement") standalone-parent 4)
        ((parent-is "switch_statement") standalone-parent 4)
        ((parent-is "case_statement") standalone-parent 4)
        ;; Function parameters and arguments
        ((parent-is "argument_list") parent-bol 4)
        ((parent-is "parameter_list") parent-bol 4)
        ;; Class/struct members use 2-space indent (Google style)
        ((parent-is "field_declaration_list") parent-bol 2)
        ((node-is "field_declaration") parent-bol 2)
        ;; Access specifiers at same level as class opening brace
        ((node-is "access_specifier") parent-bol 0)
        ;; Comments should follow the code indentation
        ((node-is "comment") no-indent)
        ;; Preprocessor directives at column 0
        ((node-is "preproc") column-0 0)
        ;; Include base k&r rules that we haven't overridden
        ,@base-style)))
  :config
  (add-hook 'c-ts-base-mode-hook
            (lambda ()
              ;; clang-format based indention
              (setq indent-line-function 'my/clang-format-on-indent
                    indent-region-function 'my/clang-format-indent-region
                    ;; for newline-and-indent (RET key binding) we fall back to
                    ;; treesit-indent, so lets disable namespace indention
                    c-ts-mode-indent-style #'my/c-ts-indent-style-no-namespace)
              ;(add-hook 'before-save-hook 'my/clang-format-buffer nil 'local)
              (electric-indent-mode -1))))

(setq google-java-format-executable "/opt/homebrew/bin/google-java-format")
(add-hook 'java-ts-mode-hook
            (lambda ()
              ;; google-java-format based indention
              (setq indent-line-function 'my/google-java-format-on-indent
                    indent-region-function 'my/google-java-format-indent-region)
              (add-hook 'before-save-hook 'google-java-format-buffer nil 'local)))

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

(setq vterm-disable-bold-font t
      vterm-disable-underline t
      ;; Add some buffer to terminal width calculation (adjust if lines still wrap)
      vterm-min-window-width 10)

;; Optimize vterm window resizing to avoid rerenders on height changes
(after! vterm
  (my/vterm-configure-resize-optimization)
  ;; Add small margins to prevent wrapping issues
  (setq-default left-margin-width 1
                right-margin-width 1))

;; Refresh vterm on window configuration changes
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (eq major-mode 'vterm-mode)
              (vterm-reset-cursor-point))))

;(advice-add 'mwheel-scroll :after #'my/scroll-mouse-handler)

(after! eat
  (setq eat-kill-buffer-on-exit t
        ;; Use xterm-256color instead of eat's custom terminfo for better powerlevel10k compatibility
        eat-term-name "xterm-256color"
        ;; Enable mouse support for clickable links
        eat-enable-mouse t)
  ;; Enable clickable URLs in eat buffers
  (add-hook 'eat-mode-hook #'goto-address-mode)
  ;; Unbind the C-x n prefix in eat-mode to allow using it for other-window
  (map! :map eat-mode-map
        "C-x n" nil)
  ;; Enable Control-left/right for word navigation in semi-char mode
  (map! :map eat-semi-char-mode-map
        "C-<left>" (lambda () (interactive) (eat-term-send-string eat-terminal "\e[1;5D"))
        "C-<right>" (lambda () (interactive) (eat-term-send-string eat-terminal "\e[1;5C"))))

(message "*** Coding / Compilation Buffer")

(add-hook 'compilation-start-hook 'my/compilation-started)
(add-hook 'compilation-finish-functions 'my/hide-compile-buffer-if-successful)

(use-package! kubernetes)

(use-package! pgmacs
  :init
  ;; local dev
  (defun my/postgres-trunk-dev ()
    (interactive)
    (pgmacs-open-string "dbname=trunk user=postgres password=password"))
)

(use-package! ejc-sql
  :config
  (setq clomacs-httpd-default-port 8595
        ejc-complete-on-dot t
        ejc-result-table-impl 'ejc-result-mode)
  (require 'ejc-autocomplete)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (auto-complete-mode t)
              (ejc-ac-setup)))
  (require 'ejc-company)
  (push 'ejc-company-backend company-backends)
  (add-hook 'ejc-sql-minor-mode-hook
            (lambda ()
              (company-mode t)))
  (add-hook 'ejc-sql-minor-mode-hook
          (lambda ()
            (ejc-eldoc-setup))))

(ejc-create-connection
 "trunk-local-dev"
 :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                    "postgresql-42.6.0.jar")
 :subprotocol "postgresql"
 :subname "//localhost:5432/trunk"
 :user "postgres"
 :password "password"
 :sslmode nil)

(ejc-create-connection
 "trunk-staging"
 :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                    "postgresql-42.6.0.jar")
 :subprotocol "postgresql"
 :subname "//syncdna-staging-rds.cvoa2ia260p9.us-east-2.rds.amazonaws.com:5432/trunk"
 :user "app"
 :password "QR0_{HN4A@Ieu5Yb<Xb8"
 :sslmode nil)

(ejc-create-connection
 "authn-staging"
 :classpath (concat "~/.m2/repository/org.postgresql/postgresql/42.6.0/"
                    "postgresql-42.6.0.jar")
 :subprotocol "postgresql"
 :subname "//syncdna-staging-rds.cvoa2ia260p9.us-east-2.rds.amazonaws.com:5432/authn"
 :user "app"
 :password "QR0_{HN4A@Ieu5Yb<Xb8"
 :sslmode nil)

(use-package! claude-code-ide
  :config
  (setq claude-code-ide-window-width 105  ; Reduced to account for fringe/margins
        claude-code-ide-use-side-window 'nil
        claude-code-ide-vterm-render-delay 0.05
        claude-code-ide-terminal-backend 'vterm
        claude-code-ide-cli-extra-flags "--dangerously-skip-permissions")
  (claude-code-ide-emacs-tools-setup))

(use-package! claude-code
  ;:bind-keymap ("C-s-x" . claude-code-command-map)
  :bind ("C-s-x" . claude-code-transient)
  :config
  (defun my-claude-notify (title message)
    "Display a macOS notification."
    (call-process "osascript" nil nil nil
                  "-e" (format "display notification \"%s\" with title \"%s\""
                               message title)))

  (setq ;;claude-code-program-switches '("--dangerously-skip-permissions")
        claude-code-notification-function #'my-claude-notify
        claude-code-confirm-kill 'nil)

  ;; Configure claude buffer display (function defined in +functions.el)
  (setq display-buffer-alist
        (cons '("^\\*claude"
                (my/claude-display-buffer)
                (inhibit-same-window . t))
              (assq-delete-all "^\\*claude" display-buffer-alist)))

  ;; Enable IDE integration support
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  ;; Focus the new claude buffer (but don't re-display it)
  (add-hook 'claude-code-start-hook
            (lambda ()
              (let ((window (get-buffer-window (current-buffer) t)))
                (when window (select-window window)))))

  (load-file "~/.config/emacs/.local/straight/repos/claude-code.el/examples/hooks/claude-code-auto-revert-hook.el")
  (setup-claude-auto-revert :git-merge t)  ; git-merge strategy, auto-save enabled

  (claude-code-mode))

(use-package! terraform
  :hook (terraform-mode . terraform-format-on-save-mode))

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
  (setq! gptel-api-key (my/read-file "~/.gptel/chatgpt.key"))

  ;; Google
  (defun gptel-gemini-api-key ()
    (my/read-file "~/.gptel/gemini.key"))
  (gptel-make-gemini "Gemini" :stream t
                     :key #'gptel-gemini-api-key)

  ;; Anthropic (default)
  (defun gptel-claude-api-key ()
    (my/read-file "~/.gptel/claude.key"))
  (setq gptel-backend
        (gptel-make-anthropic "Claude" :stream t
                              :key #'gptel-claude-api-key)))
