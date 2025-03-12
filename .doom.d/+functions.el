;;; +functions.el -*- lexical-binding: t; -*-

(defun my-read-file (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

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


;; Customize peek mode jumping so it works similar to xref jumping
(defvar my-last-file '())
(defvar my-last-pos '())
(defun my-lsp-bridge-pre-peek-jump ()
  "Store the position before executing a peek jump to jump back later."
  (push (buffer-file-name) my-last-file)
  (push (point) my-last-pos))
(defun my-lsp-bridge-post-peek-jump ()
  "Disable peek mode after a jump."
  (lsp-bridge-peek-mode -1))
(defun my-lsp-bridge-peek-jump-back ()
  "Custom function to return to the previous position of a peek jump."
  (when (not (null my-last-pos))
    (find-file (pop my-last-file))
    (goto-char (pop my-last-pos)))
  (lsp-bridge-peek-mode -1))

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

;; Compilation buffer
(defcustom auto-hide-compile-buffer-delay 1
    "Time in seconds before auto hiding compile buffer."
    :group 'compilation
    :type 'number)

(defun my-hide-compile-buffer-if-successful (buffer string)
  (setq compilation-total-time (time-subtract nil compilation-start-time))
  (setq time-str (concat " (Time: " (format-time-string "%s.%3N" compilation-total-time) "s)"))

  (if
      (with-current-buffer buffer
        (setq warnings (eval compilation-num-warnings-found))
        (setq warnings-str (concat " (Warnings: " (number-to-string warnings) ")"))
        (setq errors (eval compilation-num-errors-found))
        (setq errors-str (concat " (Errors: " (number-to-string errors) ")"))

        (if (and (eq errors 0) (string-prefix-p "finished" string)) nil t))

      ;; If errors or non-zero exit code
      (message (concat "Compiled with Errors" warnings-str errors-str time-str))

    ;; If compiled successfully or with warnings
    (progn
      (bury-buffer buffer)
      (run-with-timer auto-hide-compile-buffer-delay nil 'delete-window (get-buffer-window buffer 'visible))
      (message (concat "Compiled Successfully" warnings-str errors-str time-str)))))

(make-variable-buffer-local 'compilation-start-time)

(defun my-compilation-started (proc)
  (setq compilation-start-time (current-time)))

