;; COMMON LISP
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(require 'slime)

(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

(add-hook 'slime-repl-mode-hook
          '(lambda ()
             ;; Stop SLIME's REPL from grabbing DEL,
             ;; which is annoying when backspacing over a '('
             (define-key slime-repl-mode-map
               (read-kbd-macro paredit-backward-delete-key) nil)))

(setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
      slime-fuzzy-completion-in-place t
      slime-enable-evaluate-in-emacs t
      slime-autodoc-use-multiline-p t)

(define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
(define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
(define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)

(provide 'setup-lisp)
