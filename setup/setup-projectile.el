;;; setup-projectile.el --- Configure projectile.
;;; Commentary:
;; Obvious
;;; Code:

(setq projectile-remember-window-configs t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-indexing-method 'git)
(setq projectile-completion-system 'ido)

(provide 'setup-projectile)
