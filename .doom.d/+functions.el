;;; +functions.el -*- lexical-binding: t; -*-

(defun my-indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; leaving emacs without saving current buffer
(defun my-save-and-killbuf ()
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
(defun my-clang-format-indent ()
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

;; VI-style matching parenthesis
;;  From Eric Hendrickson edh @ med.umn.edu
(defun my-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))))

(defun my-quickload-session ()
  "Reload the last session with `doom/quickload-session` passing `t`."
  (interactive)
  (doom/quickload-session t))

(defun my-update-treemacs-icons ()
  "Replace all image (png/svg) icons in treemacs with font based icons."
  ;; Replace all special dir icons
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("test-closed" "tmp-closed" "temp-closed" "bin-closed" "git-closed" "github-closed"
                                     "public-closed" "private-closed" "screenshot-closed" "icons-closed" "readme-closed"
                                     "docs-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'treemacs-nerd-icons-file-face)
                        :extensions ("test-open" "tmp-open" "temp-open" "bin-open" "git-open" "github-open" "public-open"
                                     "private-open" "screenshot-open" "icons-open" "readme-open" "docs-open"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dred) :extensions ("src-closed" "electron-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dred) :extensions ("src-open" "electron-open"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dyellow) :extensions ("scripts-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dyellow) :extensions ("scripts-open"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dgreen) :extensions ("tests-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-dgreen) :extensions ("tests-open"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-pink) :extensions ("build-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-pink) :extensions ("build-open"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-pink) :extensions ("build-dev-closed"))
  (treemacs-create-icon :icon (propertize "		" 'face 'nerd-icons-pink) :extensions ("build-dev-open"))
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

(defun my-enable-global-lsp-bridge-mode ()
  "Add custom lsp-bridge language server configurations and enable lsp-bridge-mode"
  (dolist (hook (append lsp-bridge-default-mode-hooks acm-backend-capf-mode-hooks))
    (add-hook hook (lambda ()
                     (setq confdir "~/.lsp")
                     ;; check if a directory named ".lsp" exists in the projects root
                     (when-let (proot (projectile-project-root))
                       (let ((d (concat proot ".lsp/")))
                         (when (file-directory-p d) (setq confdir d))))
                     (when (file-directory-p confdir)
                       (setq-local lsp-bridge-user-langserver-dir confdir
                                   lsp-bridge-user-multiserver-dir confdir))
                     (lsp-bridge-mode 1)))))

(defun my-message-kill-buffer-no-query ()
  "A version of message-kill-buffer() without any user confirmation which deletes the
 file and a backup"
  (interactive nil message-mode)
  (let ((draft-article message-draft-article)
        (auto-save-file-name buffer-auto-save-file-name)
        (file-name buffer-file-name))
    (setq buffer-file-name nil)
    (kill-buffer (current-buffer))
    (ignore-errors
      (delete-file file-name))
    (ignore-errors
      (delete-file auto-save-file-name))
    (let ((message-draft-article draft-article))
      (message-disassociate-draft))))

(defun my-describe-fac-at-point (face)
  "Describe the typeface properties of FACE."
  (interactive
   (list
    (let* ((fap (plist-get (text-properties-at (point)) 'face))
           (cseq (append (when fap (list fap)) '("mode-line-inactive" "mode-line")) ))
      (completing-read "Face: " cseq nil t))))
  (describe-face face))

(defun my-org-msg-ctrl-c-ctrl-c ()
  "Send message like `message-send-and-exit'.
If the current buffer is OrgMsg buffer and OrgMsg is enabled (see
`org-msg-toggle'), it calls `message-send-and-exit'. With the
universal prefix argument, it calls `message-send'.

This is an interactive copy of the original org-msg function."
  (interactive)
  (when (eq major-mode 'org-msg-edit-mode)
    (org-msg-sanity-check)
    (if current-prefix-arg
	(org-msg-mua-call 'send 'message-send)
      (org-msg-mua-call 'send-and-exit 'message-send-and-exit))))

(defun my-scroll-mouse-handler (&rest ev)
  "A mouse scroll event handler for vterm that enables/disables vterm-copy-mode to
avoid auto-scrolling when scrolling up and turning it back on when scrolling down
to the end of the buffer."
  (let* ((win (nth 1 (car ev)))
         (pos (cadr win)))
    (if (< (buffer-size) pos)
        ;; end of the buffer, copy selection and turn copy mode off
        (when vterm-copy-mode (vterm-copy-mode-done nil))
      ;; not the end of the buffer, turn copy mode on when in vterm-mode
      (when (eq major-mode 'vterm-mode) (vterm-copy-mode 1)))))
