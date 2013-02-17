(require 'auto-complete-config)

(global-auto-complete-mode t)
(setq ac-auto-start t)
(setq ac-auto-show-menu t)

(setq ac-dwim 3)
(setq ac-override-local-map nil)

(global-set-key (kbd "C-.") 'auto-complete)

(set-default 'ac-sources '(ac-source-abbrev
                           ac-source-words-in-buffer))

(setq ac-use-menu-map t)

;; Default settings
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-menu-map "\t" 'ac-complete)
(define-key ac-menu-map "\r" nil)

(provide 'setup-auto-complete)
