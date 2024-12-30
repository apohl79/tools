;;; +bindings.el -*- lexical-binding: t; -*-

(undefine-key! "C-z" "s-w")
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
 "C-x c p" #'match-paren
 "C-c x" #'dabbrev-expand
 "C-c b" #'revert-buffer
 "C-c u" #'upcase-region
 "C-c d" #'downcase-region
 "C-c f" #'find-file-at-point
 "C-c C-a" #'auto-fill-mode
 "C-c j" #'set-justification-left
 "M-g" #'goto-line
 "C-x C-y" #'save-and-killbuf
 ;; code navigation
 "s-." #'xref-find-definitions
 "s-," #'xref-go-back
 ;"C-<return>" #'newline-and-indent
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
                   "d" #'elysium-discard-all-suggested-changes)
          "w" #'gptel
          (:prefix ("a" . "add")
                   "r" #'gptel-add
                   "f" #'gptel-add-file))
 ;; miscellaneous
 "M-s <up>" #'comint-previous-input
 "M-s <down>" #'comint-next-input
 "C-c w Q" #'my-quickload-session
 ;; mode specific
 :map (c++-mode-map c-mode-map)
 "C-c RET" #'recompile
 :map gptel-mode-map
 "C-c RET" #'gptel-menu
 "C-<return>" #'gptel-send
 "C-<up>" #'gptel-beginning-of-response
 "C-<down>" #'gptel-end-of-response
 :map vterm-mode-map
 "C-v C-c" #'vterm-send-C-c
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
