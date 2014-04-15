;; I don't need to kill emacs that easily
;; the mnemonic is C-x REALLY QUIT
(global-set-key (kbd "C-x r q") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'delete-frame)

;; Completion that uses many different methods to find options.
(global-set-key (kbd "C-.") 'hippie-expand-no-case-fold)
(global-set-key (kbd "C-:") 'hippie-expand-lines)

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Align by a regexp
(global-set-key (kbd "C-x a") 'align-regexp)

;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive
              (let ((case-fold-search isearch-case-fold-search))
                (occur (if isearch-regexp
                           isearch-string
                         (regexp-quote isearch-string)))))))

;; Expand Region
(key-chord-define-global "pn" 'er/expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-m") 'er/expand-region)

;; Multiple Cursors
(key-chord-define-global "nk" 'mc/mark-next-like-this)
(global-set-key (kbd "M-n") 'mc/mark-next-like-this)
(key-chord-define-global "kp" 'mc/mark-previous-like-this)
(global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m m") 'mc/edit-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m r") 'mc/mark-all-in-region)

;; iBuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Unset unholy keys
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key [left])
(global-unset-key [right])

;; use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])

;; Buffer file functions
(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)
(global-set-key (kbd "C-x C-r") 'eval-and-replace)

;; file the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")
(global-set-key (kbd "C-x M-j") '(lambda () (interactive) (dired-jump 1)))

;; Magit
(global-set-key (kbd "C-x g") 'magit-status) (autoload 'magit-status "magit")

;; Use smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c M-x") 'smex)

;; Give me some navigation sugar
(global-set-key [(control shift left)] 'previous-buffer)
(global-set-key [(control shift right)] 'next-buffer)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Window switching via windmove
(windmove-default-keybindings) ;; Shift+direction
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))

;; Org-mode keybindings
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Jump to my org files easily
(define-key global-map "\C-coi" '(lambda () (interactive) (find-file "~/.org/inbox.org")))
(define-key global-map "\C-cos" '(lambda () (interactive) (find-file "~/.org/someday.org")))
(define-key global-map "\C-cot" '(lambda () (interactive) (find-file "~/.org/tasks.org")))
(define-key global-map "\C-cop" '(lambda () (interactive) (find-file "~/.org/projects.org")))
(define-key global-map "\C-coj" '(lambda () (interactive) (find-file "~/.org/journal.org")))
(define-key global-map "\C-coc" '(lambda () (interactive) (find-file "~/.org/calendar.org")))
(define-key global-map "\C-cob" '(lambda () (interactive) (find-file "~/.org/business.org")))


(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

(global-set-key (kbd "C-c C-o") 'open-with)
(global-set-key (kbd "C-c g") 'search-google)
(global-set-key (kbd "C-c C-g") 'search-dict)
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c f") 'recentf-ido-find-files)
(global-set-key (kbd "C-c i") 'ido-goto-symbol)
(global-set-key (kbd "C-M-\\") 'ido-goto-symbol)
(global-set-key (kbd "C-c u") 'open-url-in-buffer)
(global-set-key (kbd "C-c C-r") 'eval-and-replace)
(global-set-key (kbd "C-c C-c") 'eval-region)
(global-set-key (kbd "C-c s") 'swap-windows)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)
(global-set-key (kbd "C-c h") 'helm-projectile)

;; Frame zooming
(global-set-key (kbd "C-M-*") 'zoom-frm-in)
(global-set-key (kbd "C-M-_") 'zoom-frm-out)

;; Ace Jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(key-chord-define-global "fg" 'ace-jump-mode)
(key-chord-define-global "fd" 'ace-jump-mode-pop-mark)
(key-chord-define-global "hj" 'undo)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(global-set-key (kbd "C-z") 'toggle-quotes)

(provide 'setup-global-bindings)
