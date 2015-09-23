(ensure-packages '(inf-ruby
                   robe
                   rinari
                   ruby-block
                   ruby-end
                   ruby-tools
                   rubocop
                   rvm))

(defun custom-ruby-hook ()
  "Define a custom ruby mode hook."
  (robe-mode)
  (push 'company-robe company-backends)
  (set (make-local-variable 'ac-delay) 2)
  (rubocop-mode))

(add-hook 'ruby-mode-hook 'custom-ruby-hook)

(provide 'setup-ruby)
;;; setup-ruby.el ends here
