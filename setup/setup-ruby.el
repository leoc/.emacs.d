(ensure-packages '(inf-ruby
                   robe
                   rinari
                   ruby-block
                   ruby-end
                   ruby-tools
                   rvm))

(defun custom-ruby-hook ()
  "Define a custom ruby mode hook."
  (robe-mode)
  (push 'ac-source-robe ac-sources)
  (set (make-local-variable 'ac-delay) 2))

(add-hook 'ruby-mode-hook 'custom-ruby-hook)

(provide 'setup-ruby)
;;; setup-ruby.el ends here
