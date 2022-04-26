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
   '((eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))
            (expand-file-name "build-dev/PluginRX/SyncDNARX_artefacts/JuceLibraryCode"
                              (projectile-project-root))
            (expand-file-name "Tests/src"
                              (projectile-project-root))
            (expand-file-name "Common/src"
                              (projectile-project-root))
            (expand-file-name "PluginBase/src"
                              (projectile-project-root))
            (expand-file-name "AudioServer/src"
                              (projectile-project-root))))
     (eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))
            (expand-file-name "build-dev/PluginTray/AudioGridderPluginTray_artefacts/JuceLibraryCode"
                              (projectile-project-root))
            (expand-file-name "Common/Source"
                              (projectile-project-root))
            (expand-file-name "../ag-deps-macos-x86_64/include"
                              (projectile-project-root))))
     (eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))
            (expand-file-name "build-dev/Tests/testrunner-rx_artefacts/JuceLibraryCode"
                              (projectile-project-root))
            (expand-file-name "Tests/src"
                              (projectile-project-root))
            (expand-file-name "Common/src"
                              (projectile-project-root))
            (expand-file-name "PluginBase/src"
                              (projectile-project-root))
            (expand-file-name "AudioServer/src"
                              (projectile-project-root))))
     (eval setq-local flycheck-clang-include-path
           (list))
     (eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))
            (expand-file-name "Tests/src"
                              (projectile-project-root))
            (expand-file-name "Common/src"
                              (projectile-project-root))
            (expand-file-name "PluginBase/src"
                              (projectile-project-root))
            (expand-file-name "AudioServer/src"
                              (projectile-project-root))
            (expand-file-name "WebSockets"
                              (projectile-project-root))
            (expand-file-name "build-dev/Tests/testrunner-rx_artefacts/JuceLibraryCode"
                              (projectile-project-root))))
     (eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))
            (expand-file-name "build-dev/Tests/testrunner-server_artefacts/JuceLibraryCode"
                              (projectile-project-root))))
     (eval setq-local flycheck-clang-include-path
           (list
            (expand-file-name "JUCE/modules"
                              (projectile-project-root))))
     (eval set-file-template! "\\.hpp$" :trigger "syncdna_hpp" :mode 'c++-mode)
     (eval yas-reload-all)
     (eval set-file-template! "\\.cpp$" :trigger "sdna_cpp" :mode 'c++-mode)
     (eval set-file-template! "\\.hpp$" :trigger "sdna_hpp" :mode 'c++-mode)
     (eval add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)
     (eval defvar private-file-templates-dir
           (expand-file-name "snippets"
                             (projectile-project-root)))
     (yas-reload-all)
     (set-file-template! "\\.cpp$" :trigger "sdna_cpp" :mode 'c++-mode)
     (add-to-list 'yas-snippet-dirs 'private-file-templates-dir 'append #'eq)
     (defvar private-file-templates-dir
       (expand-file-name "snippets"
                         (projectile-project-root)))
     (git-commit-major-mode . git-commit-elisp-text-mode)
     (projectile-compilation-dir . "/")))
 '(window-min-height 8)
 '(window-min-width 40))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
