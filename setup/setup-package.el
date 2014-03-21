;; Initialize package.el
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("org"   . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Packages should be installed in the file in which they are configured.
(package-refresh-contents)
(defun ensure-package-and-require (package &optional require-package)
  (let ((require-package (or require-package package)))
    (unless (package-installed-p package)
      (package-install package))
    (require require-package)))

(defun ensure-package (package)
  (unless (package-installed-p package)
    (package-install package)))

(defun ensure-packages (packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))))

(font-lock-add-keywords 'emacs-lisp-mode
                        '(("(\\(ensure-package-and-require\\|ensure-packages\\|ensure-package\\)" 1 font-lock-keyword-face)
                          ("(\\(ensure-package-and-require\\|ensure-package\\) '\\([^)]*\\))" 2 font-lock-constant-face)))

(provide 'setup-package)
