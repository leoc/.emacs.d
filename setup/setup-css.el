;;; setup-css.el --- Setup css development environment.

;;; Commentary:
;; Setup stylesheet environment.

;;; Code:

(ensure-packages '(scss-mode sass-mode))
(require 'css-mode)
(require 'scss-mode)

(defun custom-css-mode-hook ()
  "Set custom css defaults for SCSS and CSS mode."
  (ac-css-mode-setup)
  (setq css-indent-offset 2)
  ;; do not compile on save
  (setq scss-compile-at-save nil)
  (rainbow-mode +1))

(add-hook 'css-mode-hook 'custom-css-mode-hook)
(add-hook 'scss-mode-hook 'custom-css-mode-hook)

(add-to-list 'yank-indent-modes 'css-mode)
(add-to-list 'yank-indent-modes 'scss-mode)

(provide 'setup-css)

;;; setup-css.el ends here
