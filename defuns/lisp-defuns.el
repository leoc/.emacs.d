(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun pretty-print-lisp-region (begin end)
  "Pretty format S-expressions."
  (interactive "r")
  (save-excursion
    (goto-char (+ 1 begin))
    (while (search-forward-regexp ")[^)]" nil t)
      (backward-char)
      (forward-char)
      (insert "\n"))
    (indent-region begin end))
  (message "Ah, much better!"))

(defun remove-corresponding-elc ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun ert-silently ()
  "Evaluate buffer and run ERT silently."
  (interactive)
  (eval-buffer)
  (ert t))
