;; show the name of the current function definition in the modeline
(require 'which-func)
(which-func-mode 1)

(add-hook 'prog-mode-hook
          '(lambda ()
             (set (make-local-variable 'comment-auto-fill-only-comments) t)
             (auto-fill-mode t)
             (turn-on-whitespace)
             (turn-on-abbrev)
             (font-lock-add-keywords
              nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
                     1 font-lock-warning-face t)))
             ;; keep the whitespace decent all the time (in this buffer)
             (add-hook 'before-save-hook 'whitespace-cleanup nil t)))

(provide 'setup-programming)
