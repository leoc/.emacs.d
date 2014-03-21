;;; setup-projectile.el --- Configure projectile.
;;; Commentary:
;;; Code:
(ensure-package-and-require 'projectile)
(require 'projectile)
(projectile-global-mode)

(setq projectile-remember-window-configs t
      projectile-switch-project-action 'projectile-dired
      projectile-indexing-method 'git
      projectile-completion-system 'ido)

(provide 'setup-projectile)
;;; setup-projectile.el ends here
