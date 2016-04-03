;;; init.el --- Where all the magic begins.
;;
;; This file loads Org-mode and then loads the rest of our Emacs
;; initialization from Emacs lisp embedded in literate Org-mode files.

(package-initialize)

(let ((org-confirm-babel-evaluate nil))
  (org-babel-load-file "/home/arthur/.emacs.d/init-emacs.org")) 

;;; init.el ends here
