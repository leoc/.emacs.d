;; show the name of the current function definition in the modeline
(require 'which-func)
(which-func-mode 1)

;; Fill column indicator
(ensure-package-and-require 'fill-column-indicator)
(setq fci-rule-color "#444446")

;; diff-hl highlights changes in the buffer
(ensure-package-and-require 'diff-hl)
(defadvice git-commit-commit (after git-commit-commit-after activate)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when diff-hl-mode
        (diff-hl-update)))))

;; Set programming mode defaults
(defun custom-prog-mode-defaults ()
  "Sets custom programming defaults."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t)
  (turn-on-whitespace)
  (turn-on-abbrev)
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t)))
  ;; keep the whitespace decent all the time (in this buffer)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t)
  (turn-on-diff-hl-mode))

(add-hook 'prog-mode-hook 'custom-prog-mode-defaults)

(provide 'setup-programming)
