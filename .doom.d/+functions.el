;;; +functions.el -*- lexical-binding: t; -*-

(defun my/read-file (file-path)
  "Read the contents of FILE-PATH and return it as a string."
  (with-temp-buffer
    (insert-file-contents file-path)
    (buffer-string)))

(defun my/indent-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)
    (untabify (point-min) (point-max))))

;; leaving emacs without saving current buffer
(defun my/save-and-killbuf ()
  "Save current buffer and kill it."
  (interactive)
  (when (and (buffer-modified-p) (not buffer-read-only))
    (save-buffer))
  (kill-buffer (current-buffer)))

;; Walk between the windows
(defun my/previous-window ()
  "Previous window"
  (interactive)
  (other-window -1))

;; indent via clang-format in cc-mode
;(load! "clang-format")
;(defun my/clang-format-indent ()
;  (c-set-offset 'substatement-open 0)
;  (c-set-offset 'innamespace 0)
;  (setq tab-width 8)
;  (setq c-basic-offset 4)
;  (outline-minor-mode)
;  ; If clang-format is available, use it and deactivate electric chars
;  (when clang-format-binary-found
;    ;;(setq clang-format-style "{BasedOnStyle: Google, ColumnLimit: 120, IndentWidth: 4, AccessModifierOffset: -2, DerivePointerAlignment: false}")
;    ;; Auto indent via clang-format
;    (add-hook 'c-special-indent-hook
;              (lambda ()
;                (interactive)
;                (setq my/char-pos (buffer-substring-no-properties (point) (1+ (point))))
;                (let ((beg (if mark-active (region-beginning)
;                             (min (line-beginning-position) (1- (point-max)))))
;                      (end (if mark-active (region-end)
;                             (line-end-position))))
;                  (when (string-match-p "[^ ]" (buffer-substring-no-properties beg end)) ; ignore empty lines
;                    (when (not (equal "}" my/char-pos)) ; allow to move closing }
;                      (when (not (equal ")" my/char-pos)) ; allow to move closing )
;                        (when (not (equal "]" my/char-pos)) ; allow to move closing ]
;                          (clang-format-region beg end))))))))
;    (c-toggle-electric-state -1)))

;; indent via clang-format in c-ts-mode
(defun my/clang-format-buffer ()
  "Format the current buffer using clang-format."
  (interactive)
  (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (clang-format-buffer)))

(defun my/clang-format-region (start end)
  "Format the region from START to END using clang-format."
  (interactive "r")
  (when (derived-mode-p 'c-ts-mode 'c++-ts-mode)
    (clang-format-region start end)))

(defun my/clang-format-on-indent ()
    "Format the current line or region using clang-format."
    (interactive)
    (if (use-region-p)
        (my/clang-format-region (region-beginning) (region-end))
        (let ((pos (point))
                 (start (line-beginning-position))
                 (end (line-end-position)))
            ;; Only format if there's actual content on the line
            (if (> (- end start) 0)
                (progn
                    (my/clang-format-region start end)
                    (goto-char pos))
                (progn
                    (treesit-indent)
                    (goto-char (line-end-position)))))))

(defun my/get-clang-format-indent-width ()
  "Get the IndentWidth from clang-format configuration."
  (let* ((clang-format-dir (locate-dominating-file default-directory ".clang-format"))
         (clang-format-config (when clang-format-dir
                                (expand-file-name ".clang-format" clang-format-dir)))
         (indent-width 4)) ; default to 4 if not found
    (when (and clang-format-config (file-exists-p clang-format-config))
      (with-temp-buffer
        (insert-file-contents clang-format-config)
        (goto-char (point-min))
        (when (re-search-forward "^IndentWidth:\\s-*\\([0-9]+\\)" nil t)
          (setq indent-width (string-to-number (match-string 1))))))
    indent-width))

(defun my/newline-and-indent-no-clang-format (&optional arg)
    "Insert a newline and indent based on clang-format's style.
This avoids clang-format killing the empty line immediately."
  (interactive "*p")
  (delete-horizontal-space t)
  (unless arg
    (setq arg 1))
  (let ((electric-indent-mode nil)
        (indent-width (my/get-clang-format-indent-width)))
    (dotimes (_ arg)
      (newline nil t)
      ;; Find the first non-empty line above to get proper context
      (let* ((context-line-info (save-excursion
                                 (let ((found nil)
                                       (line-text "")
                                       (line-indent 0))
                                   (while (and (not found) (> (line-number-at-pos) 1))
                                     (forward-line -1)
                                     (setq line-text (buffer-substring-no-properties
                                                     (line-beginning-position)
                                                     (line-end-position)))
                                     (when (string-match-p "[^ \t\n]" line-text)
                                       (setq found t)
                                       (back-to-indentation)
                                       (setq line-indent (current-column))))
                                   (list line-text line-indent))))
             (prev-line (car context-line-info))
             (parent-indent (cadr context-line-info)))
        (cond
         ;; Empty line on empty line - maintain current indent level
         ((string= "" (string-trim prev-line))
          (indent-to parent-indent))
         ;; After a line ending with { or :, increase indent
         ((string-match "[{:]\\s-*$" prev-line)
          (indent-to (+ parent-indent indent-width)))
         ;; After access specifier (public:, private:, etc), indent members
         ((string-match "^\\s-*\\(public\\|private\\|protected\\):" prev-line)
          (indent-to (+ parent-indent 2))) ; Google style: members are +2 from access specifier
         ;; Lambda continuation - align with previous capture parameter
         ((and (string-match "\\[.*," prev-line)
               (not (string-match "]" prev-line)))
          (save-excursion
            (forward-line -1)
            (when (re-search-forward "\\[" (line-end-position) t)
              (indent-to (current-column)))))
         ;; Function parameter continuation - align with opening parenthesis
         ((and (string-match "(" prev-line)
               (not (string-match ")" prev-line))
               (not (string-match "{" prev-line)))
          (save-excursion
            (forward-line -1)
            (when (re-search-forward "(" (line-end-position) t)
              (indent-to (current-column)))))
         ;; Default: same indent as parent
         (t
          (indent-to parent-indent)))))))

;; Custom TAB behavior for prog-modes
(defun my/indent-or-tab ()
  "Smart tab behavior.
If region is selected, call `indent-region-function'.
If point is at end of line, call `indent-line-function'.
Otherwise call `indent-for-tab-command'."
  (interactive)
  (cond
   ;; If region is active, indent it
   ((use-region-p)
    (if indent-region-function
        (funcall indent-region-function (region-beginning) (region-end))
      (indent-region (region-beginning) (region-end))))

   ;; If at end of line, just indent this line
   ((= (point) (line-end-position))
    (if indent-line-function
        (funcall indent-line-function)
      (indent-according-to-mode)))

   ;; Otherwise standard tab behavior
   (t
    (indent-for-tab-command))))

;; Custom indent-region for C++ that avoids formatting empty lines
(defun my/clang-format-indent-region (start end)
  "Format a region using clang-format, but skip empty lines."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Only format non-empty lines
        (when (> (- line-end line-start) 0)
          (my/clang-format-region line-start line-end)))
      (forward-line 1))))

(defun my/clang-format-newline-and-indent (&optional arg)
  "Insert newline and indent using clang-format, preserving empty lines."
  (interactive "*p")
  (unless arg (setq arg 1))
  (dotimes (_ arg)
    (newline nil t)
    ;; Insert a temporary dummy statement to get proper indentation
    (insert "//")
    ;; Format the line to get proper indentation from clang-format
    (clang-format-region (line-beginning-position) (line-end-position))
    ;; Remove the "//" dummy statement
    (delete-char -2)))

(load! "google-java-format")
(defun my/google-java-format-on-indent ()
    "Format the current line or region using google-java-format."
    (interactive)
    (if (use-region-p)
        (google-java-format-region (region-beginning) (region-end))
        (let ((pos (point))
                 (start (line-beginning-position))
                 (end (line-end-position)))
            ;; Only format if there's actual content on the line
            (if (> (- end start) 0)
                (progn
                    (google-java-format-region start end)
                    (goto-char pos))
                (progn
                    (treesit-indent)
                    (goto-char (line-end-position)))))))

;; Custom indent-region for java that avoids formatting empty lines
(defun my/google-java-format-indent-region (start end)
  "Format a region using google-java-format, but skip empty lines."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (let ((line-start (line-beginning-position))
            (line-end (line-end-position)))
        ;; Only format non-empty lines
        (when (> (- line-end line-start) 0)
          (google-java-format-region line-start line-end)))
      (forward-line 1))))

(defun my/generate-cpp-implementation ()
  "Generate C++ method implementation from current line declaration."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let* ((line (buffer-substring-no-properties
                  (line-beginning-position)
                  (line-end-position)))
           (class-name (my/get-current-class-name))
           (method-sig (my/parse-method-signature line)))
      (if method-sig
          (let ((impl (my/format-method-implementation class-name method-sig)))
            (kill-new impl)
            (message "Implementation copied to clipboard: %s::%s"
                     class-name (nth 1 method-sig)))
        (message "No method signature found on current line")))))

(defun my/get-current-class-name ()
  "Get the current class name from context."
  (save-excursion
    (when (re-search-backward "^class\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" nil t)
      (match-string 1))))

(defun my/parse-method-signature (line)
  "Parse method signature from line, return (return-type method-name params)."
  ;; Handle pattern: [virtual] return_type method_name(params) [const];
  (when (string-match "^\\s-*\\(virtual\\s-+\\)?\\(.+?\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(\\([^)]*\\))\\s-*\\(const\\)?\\s-*;" line)
    (list (string-trim (match-string 2 line))  ; return type
          (string-trim (match-string 3 line))  ; method name
          (string-trim (match-string 4 line))  ; parameters
          (match-string 5 line))))             ; const

(defun my/format-method-implementation (class-name method-info)
  "Format the method implementation."
  (when (and class-name method-info)
    (let ((return-type (nth 0 method-info))
          (method-name (nth 1 method-info))
          (params (nth 2 method-info))
          (const-qualifier (nth 3 method-info)))
      (format "%s %s::%s(%s)%s {\n    // TODO: Implement\n}"
              return-type
              class-name
              method-name
              params
              (if const-qualifier " const" "")))))

;; VI-style matching parenthesis
;;  From Eric Hendrickson edh @ med.umn.edu
(defun my/match-paren (arg)
  "Go to the matching parenthesis if on parenthesis otherwise insert %."
  (interactive "p")
  (cond ((looking-at "[([{]") (forward-sexp 1) (backward-char))
        ((looking-at "[])}]") (forward-char) (backward-sexp 1))))

(defun my/quickload-session ()
  "Load the last session with a delay to allow UI rendering and show progress."
  (interactive)

  ;; Interactive mode: full UI with loading buffer and progress
    (let ((loading-buffer (get-buffer-create "*session-loading*")))
      ;; Switch to the loading buffer first
      (switch-to-buffer loading-buffer)

      (with-current-buffer loading-buffer
        (erase-buffer)
        (setq buffer-read-only nil)

        ;; Remove any margins
        (setq left-margin-width 0
              right-margin-width 0)
        (set-window-buffer (selected-window) loading-buffer)

        (insert "\n\n")
        (let ((margin "  "))
          (insert margin)
          (insert (propertize "Restoring Session" 'face `(:height 1.5 :weight bold :foreground ,(doom-color 'blue))))
          (insert "\n\n")

          (insert margin)
          (insert "Please wait while your workspace is restored")
          (insert "\n\n")

          ;; Insert progress line with a marker for updates
          (insert margin)
          (setq-local progress-start-marker (point-marker))
          (insert (propertize "0% " 'face `(:height 1.2 :foreground ,(doom-color 'green))))
          (setq-local progress-end-marker (point-marker)))

        (setq buffer-read-only t))

      (redisplay t))

    ;; Set a timer to load the session after a delay
    (run-with-timer 0.5 nil
                    (lambda ()
                      (let ((original-title frame-title-format)
                            (buffer-count 0)
                            (current-count 0)
                            (last-percentage 0))

                        ;; Count def-buffer declarations in the autosave file
                        (with-temp-buffer
                          (insert-file-contents "~/.config/emacs/.local/etc/workspaces/autosave")
                          (goto-char (point-min))
                          (while (re-search-forward "(def-buffer\\b" nil t)
                            (setq buffer-count (1+ buffer-count))))

                        ;; Initial title update
                        (setq frame-title-format "Restoring session...")
                        (sit-for 0.1)

                        ;; Advise persp-mode functions if available
                        (when (fboundp 'persp-add-buffer)
                          (advice-add 'persp-add-buffer :after
                                      (lambda (buffer &rest _)
                                        (setq current-count (1+ current-count))
                                        (let* ((percentage (if (> buffer-count 0)
                                                              (min 100 (floor (* 100 (/ (float current-count) buffer-count))))
                                                            0))
                                               (buffer-name (if (bufferp buffer)
                                                                (buffer-name buffer)
                                                              (format "%s" buffer))))
                                          (when (> percentage last-percentage)
                                            (setq last-percentage percentage)
                                            (message "Restoring session... %d%% (%d/%d) - %s"
                                                     percentage current-count buffer-count buffer-name)

                                            ;; Update the loading buffer with the current percentage
                                            (let ((loading-buf (get-buffer "*session-loading*")))
                                              (when (and loading-buf
                                                         (buffer-live-p loading-buf))
                                                (with-current-buffer loading-buf
                                                  (when (and (boundp 'progress-start-marker)
                                                             (boundp 'progress-end-marker)
                                                             (marker-position progress-start-marker)
                                                             (marker-position progress-end-marker))
                                                    (let ((inhibit-read-only t)
                                                          (percent-text (format "%d%% " percentage)))
                                                      ;; Delete old percentage text
                                                      (delete-region progress-start-marker progress-end-marker)
                                                      ;; Insert new percentage
                                                      (goto-char progress-start-marker)
                                                      (insert (propertize percent-text 'face `(:height 1.2 :foreground ,(doom-color 'green))))
                                                      ;; Update end marker
                                                      (set-marker progress-end-marker (point)))
                                                    ;; Keep loading buffer visible and force redisplay
                                                    (switch-to-buffer loading-buf)
                                                    (redisplay t))))))))))

                        (unwind-protect
                            (doom/quickload-session t)
                          ;; Switch back to loading buffer immediately to prevent flash
                          (when-let ((loading-buf (get-buffer "*session-loading*")))
                            (switch-to-buffer loading-buf))

                          ;; Cleanup
                          (when (fboundp 'persp-add-buffer)
                            (advice-remove 'persp-add-buffer (lambda (&rest _) nil)))

                          ;; Show doom dashboard after session loads
                          (run-with-timer 0.2 nil
                                          (lambda ()
                                            ;; Show dashboard
                                            (switch-to-buffer (doom-fallback-buffer))
                                            (when (fboundp '+doom-dashboard-reload)
                                              (+doom-dashboard-reload t))
                                            ;; Kill the loading buffer
                                            (when (get-buffer "*session-loading*")
                                              (kill-buffer "*session-loading*"))))

                          (setq frame-title-format original-title)
                          ;; Ensure the final title update is visible
                          (sit-for 0.1))))))

(defun my/update-treemacs-icons ()
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

(defun my/enable-global-lsp-bridge-mode ()
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
(defvar my/last-file '())
(defvar my/last-pos '())
(defun my/lsp-bridge-pre-peek-jump ()
  "Store the position before executing a peek jump to jump back later."
  (push (buffer-file-name) my/last-file)
  (push (point) my/last-pos))
(defun my/lsp-bridge-post-peek-jump ()
  "Disable peek mode after a jump."
  (lsp-bridge-peek-mode -1))
(defun my/lsp-bridge-peek-jump-back ()
  "Custom function to return to the previous position of a peek jump."
  (when (not (null my/last-pos))
    (find-file (pop my/last-file))
    (goto-char (pop my/last-pos)))
  (lsp-bridge-peek-mode -1))

(defun my/message-kill-buffer-no-query ()
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

(defun my/describe-fac-at-point (face)
  "Describe the typeface properties of FACE."
  (interactive
   (list
    (let* ((fap (plist-get (text-properties-at (point)) 'face))
           (cseq (append (when fap (list fap)) '("mode-line-inactive" "mode-line")) ))
      (completing-read "Face: " cseq nil t))))
  (describe-face face))

(defun my/org-msg-ctrl-c-ctrl-c ()
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

(defun my/org-preview ()
  "Export current org buffer to styled HTML and open in browser."
  (interactive)
  (let* ((tmp-file (make-temp-file "org-preview-" nil ".html"))
         (org-html-head
          (concat
           "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/github-markdown-css@5/github-markdown-light.min.css\">"
           "<style>"
           "body { box-sizing: border-box; min-width: 200px; max-width: 980px;"
           "       margin: 0 auto; padding: 45px; }"
           ".markdown-body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI',"
           "                 Helvetica, Arial, sans-serif; }"
           "</style>"))
         (org-html-head-include-default-style nil)
         (org-html-body-preamble (lambda (_) "<article class=\"markdown-body\">"))
         (org-html-body-epilogue (lambda (_) "</article>")))
    (org-export-to-file 'html tmp-file nil nil nil nil nil)
    (browse-url (concat "file://" tmp-file))))

(defun my/scroll-mouse-handler (&rest ev)
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

(defun my/hide-compile-buffer-if-successful (buffer string)
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

(defun my/compilation-started (proc)
  (setq compilation-start-time (current-time)))

;; vterm window resize: explicit per-window control that handles multi-vterm
;; correctly and respects copy-mode.
(defvar my/vterm-window-widths (make-hash-table :test 'eq)
  "Hash table storing the last known width of each window.")

(defun my/vterm-resize-window (window)
  "Resize the vterm terminal in WINDOW.
Always sends SIGWINCH (pty update).  Skips vterm buffer rerender when
the window is in copy-mode (user is scrolling) or only height changed."
  (with-current-buffer (window-buffer window)
    (let ((process (get-buffer-process (current-buffer))))
      (when (and process (process-live-p process))
        (let* ((margin   (if (fboundp 'vterm--get-margin-width)
                             (vterm--get-margin-width) 0))
               (width    (max (- (window-max-chars-per-line window) margin)
                               (if (boundp 'vterm-min-window-width)
                                   vterm-min-window-width 10)))
               (height   (window-body-height window))
               (stored   (gethash window my/vterm-window-widths))
               (w-changed (or (not stored) (/= width stored))))
          (puthash window width my/vterm-window-widths)
          (when (and (> width 0) (> height 0))
            ;; Always update pty dimensions → sends SIGWINCH to the process.
            (set-process-window-size process height width)
            ;; Only rerender vterm buffer when width changed and not scrolling.
            (when (and w-changed
                       (boundp 'vterm--term) vterm--term
                       (not (bound-and-true-p vterm-copy-mode)))
              (vterm--set-size vterm--term height width))))))))

(defun my/vterm-resize-all-on-size-change (frame)
  "Resize every vterm window in FRAME after a window size change."
  (dolist (window (window-list frame))
    (when (with-current-buffer (window-buffer window)
            (eq major-mode 'vterm-mode))
      (my/vterm-resize-window window))))

(defun my/vterm-configure-resize-optimization ()
  "Take over vterm resize handling for correct multi-vterm + copy-mode behaviour.
Suppresses the built-in window--adjust-process-windows path for vterm (which
processes windows sequentially and loses the second one), replacing it with an
explicit hook that resizes every visible vterm window independently."
  ;; Suppress the built-in path so we don't double-resize.
  (advice-remove 'vterm--window-adjust-process-window-size
                 #'my/vterm-window-adjust-advice)
  (advice-add 'vterm--window-adjust-process-window-size
              :override (lambda (&rest _) nil)
              '((name . my/vterm-noop-override)))
  ;; Our explicit hook handles all vterm windows in the resized frame.
  (remove-hook 'window-size-change-functions #'my/vterm-resize-all-on-size-change)
  (add-hook    'window-size-change-functions #'my/vterm-resize-all-on-size-change))

;; Dashboard workspace widget with sorting by last usage
(defun my/doom-dashboard-widget-projects ()
  "Custom widget to show all workspaces/projects as clickable buttons, sorted by last usage."
  (when (modulep! :ui workspaces)
    (let* ((workspaces (persp-names))
           (content-width 75) ; Fixed width for consistent alignment (use most of 80 chars)
           ;; Sort workspaces by last switch time (most recent first), excluding nil workspace
           (sorted-workspaces
            (sort (delq persp-nil-name (copy-sequence workspaces))
                  (lambda (a b)
                    (let* ((persp-a (persp-get-by-name a))
                           (persp-b (persp-get-by-name b))
                           (time-a (or (and persp-a (persp-parameter 'persp-last-switch-time persp-a)) 0))
                           (time-b (or (and persp-b (persp-parameter 'persp-last-switch-time persp-b)) 0)))
                      (time-less-p time-b time-a)))))) ; Sort descending (most recent first)
      (when sorted-workspaces
        (insert "\n")
        ;; Center the title using doom's centering function
        (insert (+doom-dashboard--center
                 +doom-dashboard--width
                 (propertize "Workspaces:" 'face 'doom-dashboard-menu-title)))
        (insert "\n\n")
        (dolist (workspace sorted-workspaces)
            ;; Get the project directory for this workspace
            (let* ((persp (persp-get-by-name workspace))
                   (workspace-name workspace) ; Capture for closure
                   (project-dir (when persp
                                  ;; Try to get project root from workspace buffers
                                  (let ((buffers (persp-buffers persp)))
                                    (catch 'found
                                      (dolist (buf buffers)
                                        (when-let ((root (ignore-errors
                                                          (with-current-buffer buf
                                                            (and (buffer-file-name)
                                                                 (projectile-project-root))))))
                                          ;(message "DEBUG: %s: Found project root %s from buffer %s" workspace-name root (buffer-name buf))
                                          (throw 'found (abbreviate-file-name root))))))))
                   (project-dir-display (or project-dir ""))
                   ;; Calculate spacing to right-align the path within content-width
                   (spacing (max 1 (- content-width (length workspace) (length project-dir-display)))))
              ;; Center and insert the line
              (insert
               (+doom-dashboard--center
                +doom-dashboard--width
                (with-temp-buffer
                  ;; Insert workspace name as button with workspace stored as property
                  (insert-text-button workspace
                                      'workspace-name workspace-name
                                      'action (lambda (button)
                                                 ;; Get workspace name from button property
                                                 (let ((ws (button-get button 'workspace-name)))
                                                   ;; Quit dashboard window first
                                                   (let ((dashboard-window (get-buffer-window +doom-dashboard-name)))
                                                     (when dashboard-window
                                                       (with-selected-window dashboard-window
                                                         (quit-window t))))
                                                   ;; Then switch workspace
                                                   (+workspace-switch ws t)))
                                      'follow-link t
                                      'face 'doom-dashboard-menu-desc
                                      'mouse-face 'doom-dashboard-menu-title
                                      'help-echo (format "Switch to workspace: %s → %s" workspace-name project-dir-display))
                  ;; Add spacing
                  (insert (make-string spacing ?\s))
                  ;; Add path with same color as "Doom loaded..." text
                  (let ((start (point)))
                    (insert project-dir-display)
                    (add-text-properties start (point) '(face (:foreground "#51606E"))))
                  (buffer-string))))
              (insert "\n")))))))

(defun my/update-workspace-switch-time (&rest _)
  "Update the last switch time for the current workspace."
  (when (and (bound-and-true-p persp-mode)
             (get-current-persp))
    (set-persp-parameter 'persp-last-switch-time (float-time) (get-current-persp))))

(defun my/workspace-switch-to-mru ()
  "Switch to a workspace, with candidates sorted by most recently used."
  (interactive)
  (let* ((names (cl-remove persp-nil-name (copy-sequence persp-names-cache) :count 1))
         (current (+workspace-current-name))
         ;; Remove current workspace - no point switching to where we already are
         (candidates (remove current names))
         ;; Sort by last switch time (most recent first)
         (sorted (sort candidates
                       (lambda (a b)
                         (let* ((pa (persp-get-by-name a))
                                (pb (persp-get-by-name b))
                                (ta (or (and pa (persp-parameter 'persp-last-switch-time pa)) 0))
                                (tb (or (and pb (persp-parameter 'persp-last-switch-time pb)) 0)))
                           (time-less-p tb ta)))))
         (_ (when (null sorted) (user-error "No other workspaces to switch to")))
         (choice (completing-read "Switch to workspace: "
                                  (lambda (str pred action)
                                    (if (eq action 'metadata)
                                        '(metadata (display-sort-function . identity)
                                                   (cycle-sort-function . identity))
                                      (complete-with-action action sorted str pred)))
                                  nil t)))
    (when (and choice (not (string-empty-p choice)))
      (+workspace-switch choice t)
      (+workspace/display))))

;; Extract MRU sorting (pattern already exists in my/workspace-switch-to-mru)
(defun my/workspace-names-mru-sorted ()
  "Return workspace names sorted by last switch time, most recent first."
  (when (bound-and-true-p persp-mode)
    (let ((names (cl-remove persp-nil-name (copy-sequence persp-names-cache) :count 1)))
      (sort names
            (lambda (a b)
              (let* ((pa (persp-get-by-name a))
                     (pb (persp-get-by-name b))
                     (ta (or (and pa (persp-parameter 'persp-last-switch-time pa)) 0))
                     (tb (or (and pb (persp-parameter 'persp-last-switch-time pb)) 0)))
                (time-less-p tb ta)))))))

;; tab-bar format function: returns menu-item list for clickable workspace tabs
(defun my/tab-bar-workspaces ()
  "Format function for `tab-bar-format': renders persp-mode workspaces MRU-sorted."
  (when (bound-and-true-p persp-mode)
    (let ((current (+workspace-current-name))
          (workspaces (my/workspace-names-mru-sorted)))
      (mapcar
       (lambda (name)
         (let ((face (if (equal name current)
                         'my/workspace-tab-active
                       'my/workspace-tab-inactive)))
           `(,(intern (concat "ws-" name))
             menu-item
             ,(propertize (format " %s " name) 'face face)
             (lambda () (interactive) (+workspace-switch ,name t))
             :help ,(format "Switch to workspace: %s" name))))
       workspaces))))

;; Refresh hook for tab-bar workspace display
(defun my/workspace-bar-refresh (&rest _)
  "Force tab-bar redraw after workspace state changes."
  (when (bound-and-true-p tab-bar-mode)
    (force-mode-line-update t)))

;; Frame geometry persistence
(defvar my/frame-geometry-file "~/.config/emacs/frame-geometry"
  "File to store frame geometry and font size.")

(defun my/save-frame-geometry ()
  "Save the current frame geometry and font size to file."
  (interactive)
  (let* ((frame (selected-frame))
         ;; Get the current font height (in 1/10 points)
         (current-height (face-attribute 'default :height frame))
         ;; Use character-based dimensions for more reliable restoration
         (geometry `((left . ,(frame-parameter frame 'left))
                     (top . ,(frame-parameter frame 'top))
                     (width . ,(frame-width frame))
                     (height . ,(frame-height frame))
                     (font-height . ,current-height))))
    (with-temp-file (expand-file-name my/frame-geometry-file)
      (prin1 geometry (current-buffer)))))

(defun my/restore-frame-geometry ()
  "Restore frame geometry and font size from file."
  (interactive)
  (let ((geometry-file (expand-file-name my/frame-geometry-file)))
    (when (file-exists-p geometry-file)
      (condition-case nil
          (let* ((geometry (with-temp-buffer
                            (insert-file-contents geometry-file)
                            (goto-char (point-min))
                            (read (current-buffer))))
                 (left (alist-get 'left geometry))
                 (top (alist-get 'top geometry))
                 (width (alist-get 'width geometry))
                 (height (alist-get 'height geometry))
                 (saved-font-height (alist-get 'font-height geometry)))
            ;; Restore font size first (affects frame sizing)
            (when saved-font-height
              (set-face-attribute 'default (selected-frame) :height saved-font-height))
            ;; Restore frame position and size using character dimensions
            (when (and left top width height)
              (set-frame-position (selected-frame) left top)
              (set-frame-size (selected-frame) width height nil)))
        (error nil)))))

;; Claude buffer display function
(defun my/claude-window-width ()
  "Get target width for Claude code windows.
Uses `claude-code-ide-window-width' if available, otherwise defaults to 105."
  (if (boundp 'claude-code-ide-window-width)
      claude-code-ide-window-width
    105))

(defun my/claude-display-buffer (buffer alist)
  "Display BUFFER in the other (non-active) window.
- If buffer is already displayed: use that window
- If 1 window exists: split to create 2 windows, show buffer in the new one
- If 2+ windows exist: show buffer in the other (non-selected) window
Split direction is based on frame dimensions: horizontal if width > height, vertical otherwise."
  (message "[claude-display] CALLED with buffer=%s alist=%S" buffer alist)
  ;; Check if buffer is already displayed
  (let ((existing-window (get-buffer-window buffer t)))
    (message "[claude-display] existing-window=%s" existing-window)
    (if existing-window
        (progn
          (message "[claude-display] Buffer already displayed, selecting existing window")
          (select-window existing-window)
          ;; Ensure width is correct for existing window
          (my/claude-set-window-width existing-window)
          existing-window)

      ;; Determine split direction based on frame dimensions
      (let* ((frame-w (frame-pixel-width))
             (frame-h (frame-pixel-height))
             (split-direction (if (> frame-w frame-h) 'right 'below))
             (all-windows (window-list))
             (window-count (length all-windows)))

        (message "[claude-display] frame: %dx%d, split-direction=%s, window-count=%d"
                 frame-w frame-h split-direction window-count)

        (let ((target-window
               (cond
                ;; Only 1 window - split 50/50
                ((= window-count 1)
                 (message "[claude-display] CASE: 1 window, splitting 50/50")
                 (let* ((main-window (selected-window))
                        (new-window (split-window main-window nil split-direction)))
                   (message "[claude-display] split created new-window=%s from main=%s" new-window main-window)
                   (set-window-buffer new-window buffer)
                   new-window))

                ;; 2+ windows - always use the other (non-active) window
                ((>= window-count 2)
                 (let* ((current-window (selected-window))
                        (other-windows (remove current-window all-windows))
                        (win (car other-windows)))
                   (message "[claude-display] CASE: 2+ windows, using other win=%s (keeping current=%s)" win current-window)
                   (set-window-buffer win buffer)
                   win))

                ;; Fallback - should not happen
                (t
                 (message "[claude-display] CASE: FALLBACK")
                 (set-window-buffer (selected-window) buffer)
                 (selected-window)))))

          (message "[claude-display] target-window=%s, selecting it" target-window)
          ;; Select the window (don't resize - keep existing layout)
          (select-window target-window)
          ;; Force terminal to recalculate size (delay allows window layout to settle)
          (run-with-timer 0.2 nil #'my/claude-refresh-terminal-size target-window)
          target-window)))))

(defun my/claude-set-window-width (window)
  "Set WINDOW width to target claude width if in a horizontal split."
  (when (and window (window-live-p window))
    (let ((current-width (window-width window))
          (target-width (my/claude-window-width)))
      ;; Only resize if not spanning full frame width and width differs
      (when (and (> (length (window-list)) 1)  ; More than one window
                 (not (and (window-at-side-p window 'left)
                           (window-at-side-p window 'right)))  ; Not full width
                 (/= current-width target-width))
        (ignore-errors
          (window-resize window (- target-width current-width) t))))))

(defun my/claude-refresh-terminal-size (window)
  "Force terminal in WINDOW to recalculate size (supports eat and vterm)."
  (when (and window (window-live-p window))
    ;; Force layout recalculation
    (redisplay t)
    (with-selected-window window
      (ignore-errors
        (cond
         ;; eat terminal
         ((eq major-mode 'eat-mode)
          (when (and (boundp 'eat-terminal) eat-terminal)
            ;; Resize eat terminal to match window
            (let ((inhibit-read-only t)
                  (width (window-max-chars-per-line))
                  (height (window-body-height)))
              (eat-term-resize eat-terminal width height)
              ;; Also trigger the process resize if available
              (when-let ((process (get-buffer-process (current-buffer))))
                (when (process-live-p process)
                  (set-process-window-size process height width))))))
         ;; vterm terminal
         ((eq major-mode 'vterm-mode)
          (puthash window (window-width window) my/vterm-window-widths)
          (when (and (boundp 'vterm--term) vterm--term)
            (let ((process (get-buffer-process (current-buffer))))
              (when (and process (fboundp 'vterm--window-adjust-process-window-size))
                (vterm--window-adjust-process-window-size process (list window))))
            (vterm-reset-cursor-point))))))))

(defun my/switch-to-workspace-for-directory (dir)
  "Switch to the workspace whose project root contains DIR.
Workspaces are checked in MRU order (most recently used first).
Returns the workspace name if found and switched, nil otherwise."
  (when (and (bound-and-true-p persp-mode)
             dir
             (not (string-empty-p dir)))
    (let* ((target-dir (file-name-as-directory (expand-file-name dir)))
           (names (cl-remove persp-nil-name (copy-sequence persp-names-cache) :count 1))
           (sorted (sort names
                         (lambda (a b)
                           (let* ((pa (persp-get-by-name a))
                                  (pb (persp-get-by-name b))
                                  (ta (or (and pa (persp-parameter 'persp-last-switch-time pa)) 0))
                                  (tb (or (and pb (persp-parameter 'persp-last-switch-time pb)) 0)))
                             (time-less-p tb ta))))))
      (catch 'found
        (dolist (ws-name sorted)
          (let* ((persp (persp-get-by-name ws-name))
                 (project-root
                  (when persp
                    (catch 'root
                      (dolist (buf (persp-buffers persp))
                        (when (buffer-live-p buf)
                          (when-let ((root (ignore-errors
                                            (with-current-buffer buf
                                              (and (buffer-file-name)
                                                   (projectile-project-root))))))
                            (throw 'root (file-name-as-directory (expand-file-name root))))))))))
            (when (and project-root
                       (string-prefix-p project-root target-dir))
              (+workspace-switch ws-name t)
              (select-frame-set-input-focus (selected-frame))
              (throw 'found ws-name))))))))

;;; ---------------------------------------------------------------------------
;;; Frame / session
;;; ---------------------------------------------------------------------------

(defun my/restore-session-on-first-frame (frame)
  "Restore last session on the first client frame (daemon mode).
Removes itself from `after-make-frame-functions' after the first call."
  (remove-hook 'after-make-frame-functions #'my/restore-session-on-first-frame)
  (select-frame-set-input-focus frame)
  (unless (cl-some #'buffer-file-name (buffer-list))
    (my/quickload-session)))

(defun my/setup-terminal-frame (frame)
  "Configure terminal frame for proper Unicode/icon display and key decoding."
  (with-selected-frame frame
    (unless (display-graphic-p frame)
      (menu-bar-mode -1)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      ;; M-arrows (modifier 3)
      (define-key input-decode-map "\e[1;3A" [M-up])
      (define-key input-decode-map "\e[1;3B" [M-down])
      (define-key input-decode-map "\e[1;3C" [M-right])
      (define-key input-decode-map "\e[1;3D" [M-left])
      ;; Shift+arrow (modifier 2)
      (define-key input-decode-map "\e[1;2A" [S-up])
      (define-key input-decode-map "\e[1;2B" [S-down])
      (define-key input-decode-map "\e[1;2C" [S-right])
      (define-key input-decode-map "\e[1;2D" [S-left])
      ;; Ctrl+arrow (modifier 5)
      (define-key input-decode-map "\e[1;5A" [C-up])
      (define-key input-decode-map "\e[1;5B" [C-down])
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])
      ;; Ctrl+Shift+arrow (modifier 6)
      (define-key input-decode-map "\e[1;6A" [C-S-up])
      (define-key input-decode-map "\e[1;6B" [C-S-down])
      (define-key input-decode-map "\e[1;6C" [C-S-right])
      (define-key input-decode-map "\e[1;6D" [C-S-left])
      ;; Cmd+Shift+arrow (custom modifier 10 via WezTerm)
      (define-key input-decode-map "\e[1;10D" [s-S-left])
      (define-key input-decode-map "\e[1;10C" [s-S-right])
      (redraw-frame frame))))

(defun my/close-frame-or-kill-emacs ()
  "Close the current frame.  If it's the last visible frame, kill Emacs."
  (interactive)
  (if (<= (length (visible-frame-list)) 1)
      (save-buffers-kill-emacs)
    (delete-frame)))

;;; ---------------------------------------------------------------------------
;;; Fonts
;;; ---------------------------------------------------------------------------

(defun my/configure-fontsets ()
  "Configure fontsets for unicode and symbol characters."
  (set-fontset-font t 'symbol nil)
  (set-fontset-font t 'unicode (font-spec :family my/unicode-font :size 10.5) nil 'prepend)
  (set-fontset-font t 'symbol  (font-spec :family my/unicode-font :size 10.5) nil 'prepend)
  ;; Box-drawing and geometric shapes to align vterm buffer width properly
  (set-fontset-font t '(#x2500 . #x257F) (font-spec :family my/fixed-font) nil 'prepend)
  (set-fontset-font t '(#x2580 . #x259F) (font-spec :family my/fixed-font) nil 'prepend)
  (set-fontset-font t '(#x25A0 . #x25FF) (font-spec :family my/fixed-font) nil 'prepend)
  ;; Misc symbols for terminal alignment
  (set-fontset-font t #x1D32D (font-spec :family my/fixed-font) nil 'prepend)  ; 𝌭
  (set-fontset-font t #x2387  (font-spec :family my/fixed-font) nil 'prepend)  ; ⎇
  (set-fontset-font t #x26A1  (font-spec :family my/fixed-font) nil 'prepend)  ; ⚡
  (set-face-attribute 'nobreak-space nil :underline nil))

(defun my/replace-unicode-spinners ()
  "Set buffer-local display table to replace Unicode spinners with ASCII in vterm."
  (let ((table (or buffer-display-table (make-display-table))))
    (aset table #x00B7 (vector ?⠉))  ; · Middle Dot
    (aset table #x2722 (vector ?⠉))  ; ✢ Four Teardrop-Spoked Asterisk
    (aset table #x2733 (vector ?⠒))  ; ✳ Eight Spoked Asterisk
    (aset table #x2736 (vector ?⠒))  ; ✶ Six Pointed Black Star
    (aset table #x273B (vector ?⠤))  ; ✻ Teardrop-Spoked Asterisk
    (aset table #x273D (vector ?⠤))  ; ✽ Heavy Teardrop-Spoked Asterisk
    (setq buffer-display-table table)))

;;; ---------------------------------------------------------------------------
;;; Workspace tab-bar faces
;;; ---------------------------------------------------------------------------

(defface my/workspace-tab-active
  '((t :inherit tab-bar-tab :weight bold :box nil))
  "Face for the active workspace in the tab bar.")

(defface my/workspace-tab-inactive
  '((t :inherit tab-bar-tab-inactive :weight normal :box nil))
  "Face for inactive workspaces in the tab bar.")

;;; ---------------------------------------------------------------------------
;;; Tree-sitter
;;; ---------------------------------------------------------------------------

(defun my/treesit-install-all-grammars ()
  "Install all tree-sitter grammars defined in `treesit-language-source-alist'."
  (interactive)
  (dolist (grammar treesit-language-source-alist)
    (let ((lang (car grammar)))
      (message "Installing tree-sitter grammar for %s..." lang)
      (treesit-install-language-grammar lang)))
  (message "All tree-sitter grammars installed!"))

;;; ---------------------------------------------------------------------------
;;; C++ / clang-format
;;; ---------------------------------------------------------------------------

(defun my/c-ts-indent-style-no-namespace ()
  "Custom indent style based on Google style with 4-space indentation."
  (let ((base-style (alist-get 'k&r (c-ts-mode--indent-styles 'cpp))))
    `(;; Namespace members should not be indented
      ((n-p-gp nil nil "namespace_definition") grand-parent 0)
      ;; Override k&r to use 4 spaces
      ((parent-is "compound_statement") standalone-parent 4)
      ((parent-is "if_statement")       standalone-parent 4)
      ((parent-is "else_clause")        standalone-parent 4)
      ((parent-is "do_statement")       standalone-parent 4)
      ((parent-is "for_statement")      standalone-parent 4)
      ((parent-is "while_statement")    standalone-parent 4)
      ((parent-is "switch_statement")   standalone-parent 4)
      ((parent-is "case_statement")     standalone-parent 4)
      ((parent-is "argument_list")      parent-bol 4)
      ((parent-is "parameter_list")     parent-bol 4)
      ;; Class/struct members use 2-space indent (Google style)
      ((parent-is "field_declaration_list") parent-bol 2)
      ((node-is "field_declaration")        parent-bol 2)
      ((node-is "access_specifier")         parent-bol 0)
      ((node-is "comment")    no-indent)
      ((node-is "preproc")    column-0 0)
      ,@base-style)))

;;; ---------------------------------------------------------------------------
;;; vterm extras
;;; ---------------------------------------------------------------------------

(defun my/vterm--update-ensure-unicode (orig-fn term &rest args)
  "Handle Emacs 31 unicode-string-p check in vterm--update.
Emacs 31's copy_string_contents rejects strings with eight-bit chars;
re-decode them as mac-roman (macOS clipboard encoding)."
  (condition-case err
      (apply orig-fn term args)
    (wrong-type-argument
     (if (and (eq (cadr err) 'unicode-string-p)
              args (stringp (car args)))
         (apply orig-fn term
                (decode-coding-string
                 (encode-coding-string (car args) 'raw-text-unix)
                 'mac-roman)
                (cdr args))
       (signal (car err) (cdr err))))))

(defun my/copy-to-clipboard (text)
  "Copy TEXT to system clipboard (works in terminal via pbcopy)."
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" nil "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my/vterm-mouse-select-to-clipboard (event)
  "Select text in vterm and copy to clipboard."
  (interactive "e")
  (when (eq major-mode 'vterm-mode)
    (let ((was-in-copy-mode vterm-copy-mode))
      (unless was-in-copy-mode (vterm-copy-mode 1))
      (mouse-set-region event)
      (when (use-region-p)
        (my/copy-to-clipboard
         (buffer-substring-no-properties (region-beginning) (region-end)))
        (message "Copied to clipboard"))
      (unless was-in-copy-mode
        (vterm-copy-mode -1)))))

(defun my/vterm-double-click-to-clipboard (event)
  "Select word at point in vterm and copy to clipboard."
  (interactive "e")
  (when (eq major-mode 'vterm-mode)
    (let ((was-in-copy-mode vterm-copy-mode))
      (unless was-in-copy-mode (vterm-copy-mode 1))
      (mouse-set-point event)
      (let ((bounds (bounds-of-thing-at-point 'word)))
        (when bounds
          (set-mark (car bounds))
          (goto-char (cdr bounds))
          (my/copy-to-clipboard
           (buffer-substring-no-properties (car bounds) (cdr bounds)))
          (message "Copied to clipboard")))
      (unless was-in-copy-mode
        (vterm-copy-mode -1)))))

;;; ---------------------------------------------------------------------------
;;; Databases
;;; ---------------------------------------------------------------------------

(defun my/postgres-trunk-dev ()
  "Open local dev postgres connection in pgmacs."
  (interactive)
  (pgmacs-open-string "dbname=trunk user=postgres password=password"))

;;; ---------------------------------------------------------------------------
;;; Claude Code
;;; ---------------------------------------------------------------------------

(defun my/claude-code-toggle-advice (orig-fn)
  "Use custom display function for claude-code-toggle."
  (let ((claude-code-buffer (claude-code--get-or-prompt-for-buffer)))
    (if claude-code-buffer
        (if (get-buffer-window claude-code-buffer)
            (delete-window (get-buffer-window claude-code-buffer))
          (let ((window (funcall claude-code-display-window-fn claude-code-buffer)))
            (when window
              (set-window-parameter window 'no-delete-other-windows claude-code-no-delete-other-windows)
              (when claude-code-toggle-auto-select
                (select-window window)))))
      (claude-code--show-not-running-message))))

(defun my/claude-code-vterm-make-advice (orig-fn backend buffer-name program &optional switches)
  "Preserve window configuration during vterm buffer creation.
Prevents the pop-to-buffer/delete-window dance from disrupting window layout."
  (if (eq backend 'vterm)
      (let ((window-config (current-window-configuration)))
        (prog1 (funcall orig-fn backend buffer-name program switches)
          (set-window-configuration window-config)))
    (funcall orig-fn backend buffer-name program switches)))
