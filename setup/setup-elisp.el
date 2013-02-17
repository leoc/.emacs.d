(require 'elisp-slime-nav)

(defun custom-emacs-lisp-mode-defaults ()
  (run-hooks 'prelude-lisp-coding-hook)
  (turn-on-eldoc-mode)
  (remove-corresponding-elc)
  (elisp-slime-nav-mode t)
  (rainbow-mode +1)
  (paredit-mode +1))

(add-hook 'emacs-lisp-mode-hook 'custom-emacs-lisp-mode-defaults)
(add-hook 'ielm-mode-hook  '(lambda ()
                              (custom-emacs-lisp-mode-defaults)
                              (turn-on-eldoc-mode)))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)
(define-key emacs-lisp-mode-map "\r" 'reindent-then-newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'pretty-print-lisp-region)
(define-key emacs-lisp-mode-map (kbd "C-x t") 'ert-silently)

(provide 'setup-elisp)
