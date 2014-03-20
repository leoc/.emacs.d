;;; setup-helm.el --- Setup helm.
;;; Commentary:
;;; Code:
(ensure-packages '(helm
                   helm-dash
                   helm-projectile
                   helm-grep))

(require 'eww)

(eval-after-load "helm-dash"
  '(defun helm-dash-actions (actions doc-item) `(("Go to doc" . eww))))

(provide 'setup-helm)
;;; setup-helm.el ends here
