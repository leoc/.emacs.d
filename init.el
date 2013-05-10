;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set the dir variables used by these init scripts
(setq themes-dir (expand-file-name "themes" user-emacs-directory))
(add-to-list 'load-path themes-dir)
(setq vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path vendor-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(add-to-list 'load-path defuns-dir)
(setq backups-dir (expand-file-name "backups" user-emacs-directory))
(add-to-list 'load-path backups-dir)
(setq setup-dir (expand-file-name "setup" user-emacs-directory))
(add-to-list 'load-path setup-dir)
(setq snippets-dir (expand-file-name "snippets" user-emacs-directory))
(add-to-list 'load-path snippets-dir)
(setq tmp-dir (expand-file-name "tmp" user-emacs-directory))
(add-to-list 'load-path tmp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Install packages from MELPA
(require 'setup-packages)

;; Give me less insanity
(require 'setup-sane-defaults)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'dired '(require 'setup-dired))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))
(require 'setup-org)
(require 'setup-irc)
(require 'setup-auto-complete)
(require 'setup-eproject)
(require 'setup-snippets)
(require 'setup-hippie)
(require 'setup-speedbar)

;; Setup language specific modes
(require 'setup-programming)
(require 'setup-cc)
(require 'setup-javascript)
(require 'setup-coffeescript)
(require 'setup-java)
(require 'setup-ledger)
(require 'setup-latex)
(require 'setup-lisp)
(require 'setup-tramp)
(require 'setup-elisp)
(require 'setup-ruby)
(require 'setup-slime)
(require 'setup-xml)

;; Associate extensions with their specific modes
(require 'setup-mode-mappings)

;; Functions (load all files in defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(require 'setup-css)

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Require packages that are not available automatically
(require 're-builder)
(setq reb-re-syntax 'string)
(require 'expand-region)
(require 'multiple-cursors)
(require 'key-chord)
(key-chord-mode 1)
(require 'wgrep)
(require 'smart-forward)
(require 'projectile)
(projectile-global-mode)


;; Group those buffers by vc repository root
(require 'ibuffer-vc)
(add-hook 'ibuffer-mode-hook 'ibuffer-vc-set-filter-groups-by-vc-root)

;; Fill column indicator
(require 'fill-column-indicator)
(setq fci-rule-color "#444446")

;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Setup global key-bindings
(require 'setup-global-bindings)

;; Make emacs more beautiful
(require 'setup-appearance)
