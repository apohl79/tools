(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-buffer-alist
   '((".*"
      (display-buffer-reuse-major-mode-window display-buffer-dedicated-window))))
 '(display-buffer-base-action
   '((display-buffer-dedicated-window display-buffer--maybe-same-window display-buffer-reuse-window)))
 '(safe-local-variable-values
   '((eval eval-after-load 'flycheck
           '(message "test2"))
     (eval message "test")
     (flycheck-disabled-checkers
      '(lsp))
     (c++-mode
      (flycheck-disabled-checkers
       '(lsp)))
     (c++-mode
      (flycheck-disabled-checkers c/c++-clang c/c++-gcc))
     (eval flycheck-mode nil)
     (eval setq-local lsp-clients-clangd-executable "/disabled")
     (eval seq-local lsp-clients-clangd-executable "/disabled")
     (eval setq-local ccls-executable "/usr/local/bin/ccls")
     (eval setq-local flycheck-clang-include-path
      (list))))
 '(window-min-height 8)
 '(window-min-width 40))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
