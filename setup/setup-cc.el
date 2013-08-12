(c-add-style "leoc-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))

(defun leoc-cc-mode-hook ()
  (c-set-style "leoc-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1)
  (auto-fill-mode 1)
  (hs-minor-mode 1)
  (local-set-key [return] 'newline-and-indent))

(add-hook 'c++-mode-hook 'leoc-cc-mode-hook)
(add-hook 'c-mode-hook 'leoc-cc-mode-hook)

(add-hook 'makefile-mode-hook '(lambda () (setq indent-tabs-mode t)))

(provide 'setup-cc)
