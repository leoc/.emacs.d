;;; setup-auto-completion.el --- Setup auto completion using company-mode.
;;; Commentary:
;;; Code:

(ensure-package-and-require 'company)

(add-hook 'after-init-hook 'global-company-mode)

(global-set-key [C-tab] 'company-complete)

;; Easily navigate through the completion list
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "C-j") 'company-complete-selection)

(provide 'setup-auto-completion)

;;; setup-auto-completion.el ends here
