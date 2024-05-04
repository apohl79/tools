;;; +bindings.el -*- lexical-binding: t; -*-

(map! "C-z" nil)
(setq doom-localleader-alt-key "C-z")

(map! "s-1" #'treemacs
      "s-2" #'treemacs-tag-follow-mode
      "s-." #'xref-find-definitions
      "s-," #'xref-pop-marker-stack
      "C-c C-c" #'vterm-send-C-c
      "C-c RET" #'recompile
      "s-w" nil
      "M-s-<up>" #'comint-previous-input
      "M-s-<down>" #'comint-next-input
      "<s-wheel-down>" #'enlarge-window-horizontally
      "<s-wheel-up>" #'shrink-window-horizontally
      "C-c C-e n" #'flycheck-next-error
      "C-c C-e p" #'flycheck-previous-error
      "C-s-+" #'doom/increase-font-size
      "C-s--" #'doom/decrease-font-size
      )

;; Walk between the windows
(defun my-previous-window ()
  "Previous window"
  (interactive)
  (other-window -1))
(global-set-key "\C-xp" 'my-previous-window)
(global-set-key "\C-xn" 'other-window)

;; Key bindings
(global-set-key [M-left] 'hide-subtree)
(global-set-key [M-right] 'show-children)
(global-set-key [M-up] 'hide-other)
(global-set-key [M-down] 'show-all)
(global-set-key [home] 'beginning-of-line)
(global-set-key [end] 'end-of-line)
(global-set-key "\C-xre" 'replace-regexp)
(global-set-key "\C-xcp" 'match-paren)
(global-set-key [C-return] 'newline-and-indent)
(global-set-key "\C-cx" 'dabbrev-expand)
(global-set-key "\C-cb" 'revert-buffer)
(global-set-key "\C-cu" 'upcase-region)
(global-set-key "\C-cd" 'downcase-region)
(global-set-key "\C-cf" 'find-file-at-point)
(global-set-key (kbd "C-c C-a") 'auto-fill-mode)
(global-set-key "\C-cj" 'set-justification-left)
(global-set-key "\M-g" 'goto-line)

;; leaving emacs without saving current buffer
(defun save-and-killbuf ()
  "save current buffer and quit"
  (interactive)
  ( if ( not buffer-read-only )
      (save-buffer) )
  (kill-this-buffer))
(global-set-key "\C-x\C-y" 'save-and-killbuf)

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
