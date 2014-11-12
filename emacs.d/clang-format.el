;;; Clang-format emacs integration for use with C/Objective-C/C++.

;; This defines a function clang-format-region that you can bind to a key.
;; A minimal .emacs would contain:
;;
;;   (load "<path-to-clang>/tools/clang-format/clang-format.el")
;;   (global-set-key [C-M-tab] 'clang-format-region)
;;
;; Depending on your configuration and coding style, you might need to modify
;; 'style' in clang-format, below.

(require 'json)

;; *Location of the clang-format binary. If it is on your PATH, a full path name
;; need not be specified.
(defvar clang-format-binary "clang-format")
(defvar clang-format-binary-found nil)

;; Find the binary
(setq clang-format-binary-found
      (not (equal "" (shell-command-to-string (concat "which " clang-format-binary)))))
(when (not clang-format-binary-found)
  (setq clang-format-binary "clang-format-3.5")
  (setq clang-format-binary-found
        (not (equal "" (shell-command-to-string (concat "which " clang-format-binary))))))
(when (not clang-format-binary-found)
  (setq clang-format-binary "clang-format-3.4")
  (setq clang-format-binary-found
        (not (equal "" (shell-command-to-string (concat "which " clang-format-binary))))))

;; Default style is to check for a .clang-format file
(defvar clang-format-style "file")

(defun clang-format-region ()
  "Use clang-format to format the currently active region."
  (interactive)
  (let ((beg (if mark-active
                 (region-beginning)
               (min (line-beginning-position) (1- (point-max)))))
        (end (if mark-active
                 (region-end)
               (line-end-position))))

    (clang-format beg end)))

(defun clang-format-buffer ()
  "Use clang-format to format the current buffer."
  (interactive)
  (clang-format (point-min) (point-max)))

(defun clang-format (begin end)
  "Use clang-format to format the code between BEGIN and END."
  (setq my-char-pos (buffer-substring-no-properties (point) (1+ (point))))
  (when (string-match-p "[^ ]" (buffer-substring-no-properties begin end)) ; ignore empty lines
    (when (not (equal "}" my-char-pos)) ; allow to move closing }
      (when (not (equal ")" my-char-pos)) ; allow to move closing )
        (when (not (equal "]" my-char-pos)) ; allow to move closing ]
          (let* ((orig-windows (get-buffer-window-list (current-buffer)))
                 (orig-window-starts (mapcar #'window-start orig-windows))
                 (orig-point (point))
                 (style clang-format-style))
            (unwind-protect
                (call-process-region (point-min) (point-max) clang-format-binary
                                     t (list t nil) nil
                                     "-offset" (number-to-string (1- begin))
                                     "-length" (number-to-string (- end begin))
                                     "-cursor" (number-to-string (1- (point)))
                                     "-assume-filename" (buffer-file-name)
                                     "-style" style)
              (goto-char (point-min))
              (let ((json-output (json-read-from-string
                                  (buffer-substring-no-properties
                                   (point-min) (line-beginning-position 2)))))
                (delete-region (point-min) (line-beginning-position 2))
                (goto-char (1+ (cdr (assoc 'Cursor json-output))))
                (dotimes (index (length orig-windows))
                  (set-window-start (nth index orig-windows)
                                    (nth index orig-window-starts)))))))))))
