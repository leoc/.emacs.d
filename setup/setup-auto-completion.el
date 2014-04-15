;;; setup-auto-completion.el --- Setup auto completion using company-mode.
;;; Commentary:
;;; Code:

(ensure-package-and-require 'company)
(global-company-mode 1)

(global-set-key [C-tab] 'company-complete)

;; Easily navigate through the completion list
(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "C-j") 'company-complete-selection)

(require 'color)

(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
   `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
   `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
   `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
   `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
   `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(provide 'setup-auto-completion)

;;; setup-auto-completion.el ends here
