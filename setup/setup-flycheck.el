(add-hook 'prog-mode-hook 'flycheck-mode)

(ensure-package-and-require 'flycheck-tip)
(global-set-key [up] 'flycheck-tip-cycle-reverse)
(global-set-key [down] 'flycheck-tip-cycle)

(provide 'setup-flycheck)
