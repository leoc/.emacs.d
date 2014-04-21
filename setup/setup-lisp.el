;; Configure general lisp editing
(ensure-package 'paredit)

;; Configure Common LISP environment
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(require 'slime)

;; (add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
;; (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; (add-hook 'slime-repl-mode-hook
;;           '(lambda ()
;;              ;; Stop SLIME's REPL from grabbing DEL,
;;              ;; which is annoying when backspacing over a '('
;;              (define-key slime-repl-mode-map
;;                (read-kbd-macro paredit-backward-delete-key) nil)))

;; (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
;;       slime-fuzzy-completion-in-place t
;;       slime-enable-evaluate-in-emacs t
;;       slime-autodoc-use-multiline-p t)

;; (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
;; (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
;; (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)

;; Configure elisp environment
(ensure-package 'elisp-slime-nav)
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
(define-key emacs-lisp-mode-map (kbd "C-c C-c") '(lambda ()
                                                   (interactive)
                                                   (if (region-active-p)
                                                       (progn
                                                         (eval-region (region-beginning)
                                                                      (region-end))
                                                         (deactivate-mark)))))
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'pp-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-x C-t") 'ert-silently)

(provide 'setup-lisp)
