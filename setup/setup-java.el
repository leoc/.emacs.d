(require 'eclim)
(require 'eclimd)
(require 'ac-emacs-eclim-source)

(setq eclim-executable "/home/arthur/.eclipse/org.eclipse.platform_4.2.0_1543616141/eclim")
(setq eclimd-executable "/home/arthur/.eclipse/org.eclipse.platform_4.2.0_1543616141/eclimd")
(custom-set-variables '(eclim-eclipse-dirs '("~/.eclipse")))

(define-key eclim-mode-map (kbd "C-c C-e m i") '(lambda ()
                                                  "Run maven install goal."
                                                  (interactive)
                                                  (eclim-maven-run "install")))
(define-key eclim-mode-map (kbd "C-c C-e m c") '(lambda ()
                                                  "Run maven compile goal."
                                                  (interactive)
                                                  (eclim-maven-run "compile")))

(add-hook 'java-mode-hook '(lambda ()
                             (eclim-mode)
                             (ac-emacs-eclim-java-setup)
                             (set (make-local-variable 'ac-delay) 2)
                             (setq eclim-auto-save t)
                             (setq help-at-pt-display-when-idle t)
                             (setq help-at-pt-timer-delay 0.1)
                             (help-at-pt-set-timer)
                             (setq eclim-print-debug-messages t)
                             ;; Adjust to the Eclipse styling.
                             (setq c-basic-offset 4
                                   tab-width 4
                                   indent-tabs-mode nil)
                             ;; Because eclim mode needs to save the
                             ;; buffer on completion I do not want to
                             ;; clean up the whitespaces automatically
                             ;; before saving, it simply annoys when
                             ;; your completion expands on a different
                             ;; position then you were before.
                             (set (make-local-variable 'before-save-hook) nil)
                             ;; That´s why I overwrite the binding for
                             ;; saving the buffer. Only clean up before
                             ;; saving when I hit C-x C-s.
                             (local-set-key (kbd "C-x C-s")
                                            '(lambda ()
                                               (interactive)
                                               (cleanup-buffer-safe)
                                               (save-buffer)))
                             ;; Use the normal bindings for jumping to
                             ;; declaration and popping the mark again.
                             (local-set-key (kbd "M-.") 'eclim-java-find-declaration)
                             (local-set-key (kbd "M-,") 'pop-tag-mark)))

(provide 'setup-java)
