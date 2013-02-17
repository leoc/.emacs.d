(add-hook 'coffee-mode-hook '(lambda ()
                               (set (make-local-variable 'tab-width) 2)
                               (setq coffee-js-mode 'javascript-mode)
                               (electric-indent-mode -1)))

(provide 'setup-coffeescript)
