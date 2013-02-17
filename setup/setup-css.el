(add-hook 'css-mode-hook '(lambda ()
                            (setq css-indent-offset 2)
                            (rainbow-mode +1)))

;; do not comepile on save when in less-mode
(setq less-compile-at-save nil)
(setq scss-compile-at-save nil)

(provide 'setup-css)
