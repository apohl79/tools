(message "Loading configuration...")

(load! "+functions")

(setq user-full-name "Andreas Pohl"
      user-mail-address "pohl@e47.org")

;; Load custom projects system (replaces persp-mode/workspaces)
(load! "projects")

;; Restore frame geometry and font size from last session
(add-hook 'doom-init-ui-hook #'my/restore-frame-geometry)

;; Save frame geometry and font size on exit
(add-hook 'kill-emacs-hook #'my/save-frame-geometry)

;; Restore last session automatically (skip when files were passed on command line).
;; Non-daemon: restore after init (frame exists for loading UI).
;; Daemon: defer to first client frame via after-make-frame-functions,
;; since restoring during daemon init breaks the daemon (no frame for UI).
;; my/restore-session-on-first-frame is defined in +functions.el
(if (daemonp)
    (add-hook 'after-make-frame-functions #'my/restore-session-on-first-frame)
  (add-hook 'doom-after-init-hook
            (lambda ()
              (unless (cl-some #'buffer-file-name (buffer-list))
                ;; Use a 0-second timer so Emacs returns to the event loop first
                ;; (frame must be painted before the loading screen can appear)
                (run-with-timer 0 nil #'projects-restore)
                ;; Ensure a default layout is set if restore doesn't set one
                (run-with-timer 0.5 nil
                                (lambda ()
                                  (unless (frame-parameter nil 'projects-multi-layout)
                                    (projects--set-multi-layout "1x1")))))))))


(defun my/projects-setup-client-frame ()
  "Show project info buffer for current project when emacsclient creates a frame.
Sets default-directory to the project root and marks the frame as fresh so
that opening a terminal (vterm/eat/claude) collapses it to fullscreen."
  (when (and projects--current
             (not (frame-parameter (selected-frame) 'parent-frame)))
    (let* ((frame     (selected-frame))
           (frame-win (frame-selected-window frame))
           (info-buf  (projects--create-info-buffer projects--current))
           (dir       (projects-dir projects--current)))
      ;; with-selected-window ensures current-buffer changes inside the hook
      ;; (server hooks run in save-current-buffer context)
      (with-selected-window frame-win
        (switch-to-buffer info-buf))
      (when dir
        (with-current-buffer info-buf
          (setq-local default-directory dir))
        (setq-default default-directory dir))
      (set-frame-parameter frame 'projects-fresh-client t))))

(remove-hook 'server-after-make-frame-hook #'my/projects-setup-client-frame)
(add-hook    'server-after-make-frame-hook #'my/projects-setup-client-frame)


;; Ensure proper terminal setup for emacsclient frames (defined in +functions.el).
(add-hook 'after-make-frame-functions #'my/setup-terminal-frame)


;; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Disable the quit question
(setq confirm-kill-emacs nil)

;; Make sure we get asked to accept non-safe local variables from .dir-locals.el files
(setq enable-local-variables t)

;; "ctrl - left click" buffer menu: increase number of items shown.
;; Set max length of this list. default 20. see next.
(setq mouse-buffer-menu-maxlen 30)

;; set # buffer in a mode before grouping begins. takes precedence over previous
;; set to 1 to always group by mode. default 4
(setq mouse-buffer-menu-mode-mult 1)

;; Enable pixel scrolling
(pixel-scroll-precision-mode 1)

;; Adjust scrolling speed (optional)
(setq pixel-scroll-precision-large-scroll-height 40.0)

;; Disable momentum scrolling if you prefer (optional)
(setq pixel-scroll-precision-use-momentum nil)

;; Do not truncate lines but wrap them into the next line
(setq-default truncate-lines nil)

;; Desired line length
(setq-default fill-column 120)

;; File name in the mode line
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)

;; C-x C-c: close current frame, or kill Emacs if it's the last one (defined in +functions.el).
(global-set-key (kbd "C-x C-c") #'my/close-frame-or-kill-emacs)


;; Add the projects widget to the dashboard (without the shortmenu)
;; Widget function is defined in +functions.el
(setq +doom-dashboard-functions
      '(doom-dashboard-widget-banner
        my/doom-dashboard-widget-projects
        doom-dashboard-widget-loaded))


;; Disable line numbers in dashboard - multiple approaches for reliability
(add-hook '+doom-dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

(add-hook 'doom-dashboard-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (setq-local display-line-numbers nil)))

;; Workspace tab bar faces (defined in +functions.el)
;; Theme-aware colors (`:height` is ignored in TTY, which is fine)
(add-hook 'doom-load-theme-hook
  (lambda ()
    (set-face-attribute 'tab-bar nil :background (face-foreground 'vertical-border nil t) :box nil :height 0.9)
    (set-face-attribute 'my/workspace-tab-active nil
                        :background (doom-color 'blue) :foreground (doom-color 'bg) :weight 'bold)
    (set-face-attribute 'my/workspace-tab-inactive nil
                        :background (face-foreground 'vertical-border nil t) :foreground "#B8860B")))

;; Preserve multi-project window layout when transient menus open/close.
;; save-window-configuration causes buffer-assignment conflicts that trigger
;; registration ping-pong. Instead save/restore only line-based window heights.
(defvar my/projects-pre-transient-sizes nil)

(defun my/projects-transient-restore-sizes ()
  (when my/projects-pre-transient-sizes
    (let ((saved my/projects-pre-transient-sizes))
      (setq my/projects-pre-transient-sizes nil)
      (run-with-timer
       0.1 nil
       (lambda ()
         (dolist (entry saved)
           (let* ((w       (car entry))
                  (target  (cdr entry))
                  (delta   (- target (window-total-height w))))
             (when (and (window-live-p w) (/= delta 0))
               (ignore-errors (window-resize w delta))))))))))

(after! transient
  (defun my/projects-transient--pre-show ()
    (when (not (window-live-p transient--window))
      (setq my/projects-pre-transient-sizes
            (delq nil
                  (mapcar (lambda (w)
                            (unless (window-parameter w 'window-side)
                              (cons w (window-total-height w))))
                          (window-list nil 0))))))
  (advice-add 'transient--show :before #'my/projects-transient--pre-show)
  (add-hook 'transient-post-exit-hook #'my/projects-transient-restore-sizes))

;; Projects tab-bar integration
;; Uses my/workspace-tab-active / my/workspace-tab-inactive faces (defined in +functions.el)
(after! projects
  (setq tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-show t
        tab-bar-format '(projects--tab-bar-format))
  (tab-bar-mode 1)
  (add-hook 'projects-switch-hook #'projects--tab-bar-refresh))


(setq org-directory "~/ownCloud/org"
      org-agenda-files (directory-files-recursively org-directory "\\.org$")
      org-support-shift-select t
      org-replace-disputed-keys t
      org-startup-indented t
      org-pretty-entities t
      org-use-sub-superscripts "{}"
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Unbind keys to allow standard behavior
(map! :after org
      :map org-mode-map
      "C-x n" nil           ; allow other-window
      "C-S-<up>" nil        ; allow block selection
      "C-S-<down>" nil
      :localleader
      "p" #'my/org-preview)

(after! markdown-mode
  ;; Unbind the C-x n prefix to allow using it for other-window
  (map! :map markdown-mode-map
        "C-x n" nil)

  ;; Use pandoc with GFM for proper table rendering
  (setq markdown-command "pandoc -f gfm -t html5")

  ;; GitHub light theme for preview
  (setq markdown-css-paths
        '("https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown-light.min.css"))
  (setq markdown-xhtml-header-content
        (concat
         "<style>
          body { box-sizing: border-box; min-width: 200px; max-width: 980px;
                 margin: 0 auto; padding: 45px; }
          .markdown-body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',
                           Helvetica, Arial, sans-serif; }
        </style>"
         my/mermaid-head-extra))
  (setq markdown-xhtml-body-preamble "<article class=\"markdown-body\">")
  (setq markdown-xhtml-body-epilogue
        (concat "</article>" my/mermaid-init-script))

  (add-hook! 'markdown-mode-hook
             ;; Org mode tables in markdown
             #'orgtbl-mode
             ;; Make URLs clickable with mouse and C-c RET
             #'goto-address-mode))

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
 "s-S-<left>" #'enlarge-window-horizontally
 "s-S-<right>" #'shrink-window-horizontally
 "s-*" #'doom/increase-font-size
 "s-_" #'doom/decrease-font-size

;; claude-code
;;(:prefix ("C-s-x" . "Claude")
;;         "c" #'claude-code-ide
;;         "r" #'claude-code-ide-resume
;;         "k" #'claude-code-ide-stop
;;         "RET" #'claude-code-ide-insert-newline
;;         "ESC" #'claude-code-ide-send-escape)

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
 :map python-ts-mode-map
 "C-M-x" nil  ;; Let C-M-x pass through to global (claude-code-transient)
 :map emacs-lisp-mode-map
 "C-M-x" nil  ;; Let C-M-x pass through to global (claude-code-transient)
 :map vterm-mode-map
 "C-c C-c" #'vterm-send-C-c
 "C-M-x" nil  ;; Let C-M-x pass through to global (claude-code-transient)
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

 ;; Projects system keybindings — uses :leader so both SPC w and C-c w work.
 :leader
 (:prefix ("w" . "projects")
  :desc "Switch project"             "w" #'projects-switch
  :desc "New project"                "n" #'projects-create
  :desc "Rename project"             "r" #'projects-rename
  :desc "Delete project"             "k" #'projects-delete
  :desc "Move buffer to project"     "m" #'projects-move-buffer
  :desc "Switch buffer in project"   "b" #'projects-switch-buffer
  :desc "Save projects state"        "s" #'projects-save
  :desc "Restore projects session"   "R" #'projects-restore
  :desc "Restore from backup…"       "l" (lambda () (interactive) (let ((current-prefix-arg t)) (call-interactively #'projects-restore)))
  :desc "Project info buffer"        "i" #'projects-show-info
  :desc "Clone from git"             "g" #'projects-clone-from-git
  :desc "Layout: 1x1"                "1" (lambda () (interactive) (projects-set-layout "1x1"))
  :desc "Layout: 2x1"                "2" (lambda () (interactive) (projects-set-layout "2x1"))
  :desc "Layout: 2x2"                "3" (lambda () (interactive) (projects-set-layout "2x2"))
  :desc "Layout: 3x2"                "4" (lambda () (interactive) (projects-set-layout "3x2")))
 )

;; Remove leftover Doom workspace/winner bindings from the w prefix
(after! winner
  (undefine-key! :keymaps 'doom-leader-map "w u" "w U"))
(after! doom-keybinds
  (undefine-key! :keymaps 'doom-leader-map "w TAB"))

;; Override C-x b to use project-aware buffer switching
(map! "C-x b"       #'projects-switch-buffer
      "M-TAB"       #'projects-switch-dispatch
      "C-c C-x"     #'projects-show-info)

(map! :map vertico-map
      "M-TAB" #'vertico-next)

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

;; my/configure-fontsets defined in +functions.el
;; Also apply when loading vterm/eat buffers (GUI only - terminal uses its own fonts)
(when (display-graphic-p)
  (add-hook 'after-setting-font-hook #'my/configure-fontsets)
  (add-hook 'doom-init-ui-hook #'my/configure-fontsets)
  (add-hook 'vterm-mode-hook #'my/configure-fontsets)
  (add-hook 'eat-mode-hook #'my/configure-fontsets))

;; my/replace-unicode-spinners defined in +functions.el
;; Replace spinners only in GUI mode
(when (display-graphic-p)
  (add-hook 'vterm-mode-hook #'my/replace-unicode-spinners)
  (add-hook 'eat-mode-hook #'my/replace-unicode-spinners))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Always fixed font even in variable-pitch-mode
(set-face-attribute 'line-number nil :font my/fixed-font)
(set-face-attribute 'line-number-current-line nil :font my/fixed-font)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))
;; Re-enable modeline in vterm (Doom's vterm module hides it via hide-mode-line-mode)
(add-hook 'vterm-mode-hook (lambda () (hide-mode-line-mode -1)) 90)
(add-hook 'eat-mode-hook (lambda () (display-line-numbers-mode 0)))

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


;; In multi-view mode, show vertico candidates inside the project window
;; (same window that triggered completing-read) instead of growing the
;; minibuffer and disrupting the layout.
;;
;; vertico-buffer--setup uses with-minibuffer-selected-window, so
;; display-buffer-same-window places candidates in the project window.
;; Our global minibuffer-setup-hook fires before vertico's buffer-local
;; setup hook, so enabling vertico-buffer-mode here takes effect.
(defvar my/vertico-buffer-enabled-count 0
  "Nesting counter: how many multi-view minibuffer sessions are active.")

(defun my/vertico-buffer-setup ()
  (cl-incf my/vertico-buffer-enabled-count)
  (unless vertico-buffer-mode
    (vertico-buffer-mode 1)))

(defun my/vertico-buffer-teardown ()
  (when (> my/vertico-buffer-enabled-count 0)
    (cl-decf my/vertico-buffer-enabled-count)
    (when (and (= my/vertico-buffer-enabled-count 0)
               vertico-buffer-mode)
      (vertico-buffer-mode -1))))

(after! vertico
  (require 'vertico-buffer)
  (setq vertico-buffer-display-action '(display-buffer-same-window))
  (add-hook 'minibuffer-setup-hook #'my/vertico-buffer-setup)
  ;; Depth 100: run after vertico-buffer's own restore hook (added per-session
  ;; at default depth), so the project window is already restored before we
  ;; turn off vertico-buffer-mode. Without this, (vertico-buffer-mode -1) fires
  ;; first and the restore logic finds the mode already off and skips cleanup,
  ;; leaving the minibuffer buffer sitting in the project window.
  (add-hook 'minibuffer-exit-hook  #'my/vertico-buffer-teardown 100))

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
        org-modern-table nil
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

(after! ibuffer
  ;; Group buffers by project directory.
  ;; :append ensures we run after ibuffer-projectile (which loads lazily and
  ;; adds its own hook mid-flight on first ibuffer open, overwriting ours).
  (add-hook 'ibuffer-hook #'projects--ibuffer-setup :append)
  (setq ibuffer-formats
        `((mark modified read-only locked
                " " (icon 2 2 :left :elide)
                ,(propertize " " 'display `(space :align-to 8))
                (name 55 55 :left :elide)
                " " (size 9 -1 :right)
                " " (mode 16 16 :left :elide)
                ,@(when (require 'ibuffer-vc nil t)
                    '(" " (vc-status 12 :left)))
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))


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


;; Make the git summary line longer
(after! magit
  (setq git-commit-summary-max-length 120))

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
        ;; Use homebrews clangd
        ;;lsp-clients-clangd-executable "/opt/homebrew/Cellar/llvm@19/19.1.7/bin/clangd"
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
          (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (bash "https://github.com/tree-sitter/tree-sitter-bash")))

  ;; my/treesit-install-all-grammars defined in +functions.el
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
  ;; my/c-ts-indent-style-no-namespace defined in +functions.el
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


(setq vterm-disable-bold-font t
      vterm-disable-underline t
      ;; Add some buffer to terminal width calculation (adjust if lines still wrap)
      vterm-min-window-width 10)

;; Optimize vterm window resizing to avoid rerenders on height changes
(after! vterm
  (my/vterm-configure-resize-optimization)

  ;; my/vterm--update-ensure-unicode defined in +functions.el
  (advice-add 'vterm--update :around #'my/vterm--update-ensure-unicode)

  ;; Add small margins to prevent wrapping issues
  (setq-default left-margin-width 1
                right-margin-width 1)
  ;; Override vterm--self-insert-meta for keys we need in claude-code
  ;; S-return arrives as M-RET - send CSI u encoding for newline in Claude
  (define-key vterm-mode-map (kbd "M-RET") (lambda () (interactive) (vterm-send-string "\e[13;2u")))
  ;; M-TAB: override vterm--self-insert-meta so OPT+TAB switches projects
  (define-key vterm-mode-map (kbd "M-TAB") #'projects-switch-dispatch)
  ;; Enable clickable URLs in vterm buffers
  (add-hook 'vterm-mode-hook #'goto-address-mode)
  ;; Enable clickable file paths in vterm buffers (my/vterm-file-link-* in +functions.el)
  (add-hook 'vterm-mode-hook #'my/vterm-file-link-setup)

  ;; my/copy-to-clipboard, my/vterm-mouse-select-to-clipboard,
  ;; my/vterm-double-click-to-clipboard defined in +functions.el
  (define-key vterm-mode-map [down-mouse-1] nil)
  (define-key vterm-mode-map [drag-mouse-1] #'my/vterm-mouse-select-to-clipboard)
  (define-key vterm-mode-map [double-mouse-1] #'my/vterm-double-click-to-clipboard))

;; Refresh vterm on window configuration changes
(add-hook 'window-configuration-change-hook #'my/vterm-reset-cursor-point-maybe)

;(advice-add 'mwheel-scroll :after #'my/scroll-mouse-handler)

(after! eat
  (setq eat-kill-buffer-on-exit t
        ;; Use xterm-256color instead of eat's custom terminfo for better powerlevel10k compatibility
        eat-term-name "xterm-256color"
        ;; Enable mouse support for clickable links
        eat-enable-mouse t)
  ;; Enable clickable URLs in eat buffers
  (add-hook 'eat-mode-hook #'goto-address-mode)
  ;; Disable flycheck in terminal buffers (makes no sense there)
  (add-hook 'eat-mode-hook (lambda () (flycheck-mode -1)))
  ;; Unbind the C-x n prefix in eat-mode to allow using it for other-window
  (map! :map eat-mode-map
        "C-x n" nil)
  ;; Enable Control-left/right for word navigation in semi-char mode
  (map! :map eat-semi-char-mode-map
        "C-M-x" nil  ;; Let C-M-x pass through to global (claude-code-transient)
        "C-<left>" (lambda () (interactive) (eat-term-send-string eat-terminal "\e[1;5D"))
        "C-<right>" (lambda () (interactive) (eat-term-send-string eat-terminal "\e[1;5C"))))

(use-package! kubernetes)

(use-package! claude-code
  ;:bind-keymap ("C-s-x" . claude-code-command-map)
  :config
  ;; Bind C-M-x globally via global-set-key (not :bind) so mode-specific
  ;; bindings like eval-defun in emacs-lisp-mode take precedence.
  (global-set-key (kbd "C-M-x") claude-code-command-map)
  (setq claude-code-program-switches '("--dangerously-skip-permissions")
        claude-code-confirm-kill 'nil
        claude-code-terminal-backend 'vterm  ; use vterm to avoid eat charset errors
        claude-code-enable-notifications nil)  ; disable - vterm bell detection is broken

  ;; Configure claude buffer display (function defined in +functions.el)
  ;; Use claude-code-display-window-fn directly instead of display-buffer-alist
  ;; to avoid vterm's pop-to-buffer/bury-buffer dance interfering with window layout
  (setq claude-code-display-window-fn
        (lambda (buffer) (my/claude-display-buffer buffer nil)))

  ;; my/claude-code-toggle-advice, my/claude-code-vterm-make-advice defined in +functions.el
  (advice-add 'claude-code-toggle    :around #'my/claude-code-toggle-advice)
  (advice-add 'claude-code--term-make :around #'my/claude-code-vterm-make-advice)

  ;; Disable vterm bell detector - it triggers on OSC sequences causing spam
  ;; Terminal output arrives in chunks so regex filtering doesn't work reliably
  (advice-remove 'vterm--filter #'claude-code--vterm-bell-detector)

  ;; Enable IDE integration support
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)

  ;; Disable IDE file resources in @ picker (no ide:// entries)
  (setq monet-get-open-editors-tool
        (lambda ()
          (list `((type . "text")
                  (text . ,(json-encode '((editors . []))))))))
  (advice-add 'monet--get-file-resources :override (lambda (&optional _cursor) nil))

  ;; Focus the new claude buffer (but don't re-display it)
  (add-hook 'claude-code-start-hook
            (lambda ()
              (let ((window (get-buffer-window (current-buffer) t)))
                (when window (select-window window)))))

  (claude-code-mode))

(use-package! terraform
  :hook (terraform-mode . terraform-format-on-save-mode))
