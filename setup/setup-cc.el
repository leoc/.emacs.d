(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-offset 'member-init-intro '++)
             (setq indent-tabs-mode t)
             (setq c-basic-offset 4)
             (setq tab-width 4)
             (c-set-style "bsd")
             (auto-fill-mode 1)
             (hs-minor-mode 1)
             (local-set-key [return] 'newline-and-indent)))

(add-hook 'makefile-mode-hook '(lambda ()
                                 (setq indent-tabs-mode t)))

(provide 'setup-cc)
