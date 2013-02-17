(add-hook 'js-mode-hook '(lambda ()
                           ;; electric-layout-mode doesn´t work with js-mode
                           (electric-layout-mode -1)))

(provide 'setup-javascript)
