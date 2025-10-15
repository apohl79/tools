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
  "save current buffer and quit"
  (interactive)
  (if (not buffer-read-only)
      (save-buffer))
  (kill-this-buffer))

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

  ;; Create a dedicated loading buffer (not the dashboard buffer)
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

      ;; Insert text with display properties to center it
      (insert "\n\n\n\n")

      ;; Calculate padding to center "Loading Session..."
      (let* ((text1 "Loading Session...")
             (text2 "Please wait while your workspace is restored")
             (text3 "0%")
             (padding1 (propertize " " 'display `(space :align-to (- center ,(/ (length text1) 2)))))
             (padding2 (propertize " " 'display `(space :align-to (- center ,(/ (length text2) 2)))))
             (padding3 (propertize " " 'display `(space :align-to (- center ,(/ (length text3) 2))))))

        (insert padding1)
        (insert (propertize text1 'face '(:height 1.5 :weight bold)))
        (insert "\n\n")

        (insert padding2)
        (insert text2)
        (insert "\n\n")

        ;; Insert progress line with a marker for updates
        (insert padding3)
        (setq-local progress-start-marker (point-marker))
        (insert (propertize text3 'face '(:height 1.2)))
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
                     (sit-for 0.1) ;; Allow the title to update

                     ;; Advise persp-mode functions if available
                     (when (fboundp 'persp-add-buffer)
                       (advice-add 'persp-add-buffer :after
                                (lambda (&rest _)
                                  (setq current-count (1+ current-count))
                                  (let ((percentage (if (> buffer-count 0)
                                                      (min 100 (floor (* 100 (/ (float current-count) buffer-count))))
                                                      0)))
                                    (when (> percentage last-percentage)
                                      (setq last-percentage percentage)
                                      (setq progress (format "Restoring session... %d%% (%d/%d)"
                                                       percentage current-count buffer-count))
                                      (message "%s" progress)

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
                                                    (percent-text (format "%d%%" percentage)))
                                                ;; Delete old percentage text
                                                (delete-region progress-start-marker progress-end-marker)
                                                ;; Insert new percentage
                                                (goto-char progress-start-marker)
                                                (insert (propertize percent-text 'face '(:height 1.2)))
                                                ;; Update end marker
                                                (set-marker progress-end-marker (point)))
                                              ;; Force redisplay
                                              (redisplay t))))))))))

                     (unwind-protect
                         (doom/quickload-session t)
                       ;; Cleanup
                       (when (fboundp 'persp-add-buffer)
                         (advice-remove 'persp-add-buffer (lambda (&rest _) nil)))
                       (setq frame-title-format original-title)
                       (sit-for 0.1) ;; Ensure the final title update is visible

                       ;; Show doom dashboard after session loads
                       (run-with-timer 0.2 nil
                                       (lambda ()
                                         ;; Kill the loading buffer
                                         (when (get-buffer "*session-loading*")
                                           (kill-buffer "*session-loading*"))
                                         ;; Show dashboard
                                         (switch-to-buffer (doom-fallback-buffer))
                                         (when (fboundp '+doom-dashboard-reload)
                                           (+doom-dashboard-reload t)))))))))

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

;; vterm window resize optimization
(defvar my/vterm-window-widths (make-hash-table :test 'eq)
  "Hash table storing the last known width of each window.")

(defun my/vterm-window-adjust-advice (orig-fun &rest args)
  "Advice to only signal vterm window size change on width change.
This prevents unnecessary terminal reflows when only height changes."
  (let ((result (apply orig-fun args)))
    (let ((width-changed nil))
      ;; Check all windows for vterm buffers
      (dolist (window (window-list))
        (let ((buffer (window-buffer window)))
          (when (and buffer 
                     (with-current-buffer buffer
                       (eq major-mode 'vterm-mode)))
            (let ((current-width (window-width window))
                  (stored-width (gethash window my/vterm-window-widths)))
              (when (or (not stored-width) (/= current-width stored-width))
                (setq width-changed t)
                (puthash window current-width my/vterm-window-widths))))))
      ;; Only return result if width actually changed
      (if width-changed
          result
        nil))))

(defun my/vterm-configure-resize-optimization ()
  "Configure vterm to only rerender on width changes, not height changes."
  (advice-add 'vterm--window-adjust-process-window-size
              :around #'my/vterm-window-adjust-advice))

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
