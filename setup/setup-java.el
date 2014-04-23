(ensure-package 'emacs-eclim)

(require 'eclim)
(require 'eclimd)
(require 'company-emacs-eclim)

(setq eclim-eclipse-dirs '("~/.eclipse"))
(setq eclim-executable (expand-file-name "~/.eclipse/eclim"))
(setq eclim-auto-save t)
(setq eclimd-executable (expand-file-name "~/.eclipse/eclimd"))
(setq eclimd-default-workspace (expand-file-name "~/projects"))

(global-eclim-mode)

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
                             (company-emacs-eclim-setup)
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

(ensure-package 'android-mode)
(require 'android-mode)

(setq android-mode-key-prefix "\C-c \C-p")

(android-defun-ant-task "debug install")
(android-defun-maven-task "install android:deploy")

(defun android-build-debug-and-install ()
  "Build the application in a debug mode."
  (interactive)
  (funcall (case android-mode-builder
             ('ant 'android-ant-debug-install)
             ('maven 'android-maven-install-android-deploy))))

(define-key android-mode-map (kbd "C-c C-p C-i") 'android-build-debug-and-install)

(provide 'setup-java)
