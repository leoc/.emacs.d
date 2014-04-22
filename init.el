;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set the dir variables used by these init scripts
(defvar vendor-dir (expand-file-name "vendor" user-emacs-directory)
  "Specifies where vendored libs can be found.")
(add-to-list 'load-path vendor-dir)
(defvar defuns-dir (expand-file-name "defuns" user-emacs-directory)
  "Specifies the directory with additional defuns.")
(add-to-list 'load-path defuns-dir)
(defvar backups-dir (expand-file-name "backups" user-emacs-directory)
  "Specifies the directory to save backups in.")
(add-to-list 'load-path backups-dir)
(defvar setup-dir (expand-file-name "setup" user-emacs-directory)
  "Specifies the directory with the configuration files.")
(add-to-list 'load-path setup-dir)
(defvar snippets-dir (expand-file-name "snippets" user-emacs-directory)
  "Specifies the yasnippet snippet directory.")
(add-to-list 'load-path snippets-dir)
(defvar tmp-dir (expand-file-name "tmp" user-emacs-directory)
  "Specifies the temp directory.")
(add-to-list 'load-path tmp-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Use cl macros like loop and defun*
(eval-when-compile (require 'cl))

;; Setup package.el and helper functions
(require 'setup-package)

;; Functions (load all files in defuns-dir)
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; TODO: the following packages should be required and installed by
;; there respective packages.
(ensure-packages '(alert markup popup frame-fns s f kv))

;; Give me less insanity and setup the editor wisely
(require 'setup-sane-defaults)
(require 'setup-editor)

;; Setup extensions
(require 'setup-ido)
(require 'setup-dired)
(require 'setup-git)
(require 'setup-grep)
(require 'setup-shell)
(require 'setup-org)
(require 'setup-irc)
(require 'setup-auto-completion)
(require 'setup-projectile)
(require 'setup-snippets)
(require 'setup-hippie)
(require 'setup-speedbar)
(require 'setup-tramp)
(require 'setup-mail)

;; Flycheck everywhere!
(ensure-package-and-require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Require regex builder to conveniently build regular expressions
(require 're-builder)
(setq reb-re-syntax 'string)

;; In some modes it is interesting to see RGB colors directly
(ensure-package-and-require 'rainbow-mode)

;; Setup language specific modes
(require 'setup-programming)
(require 'setup-cc)
(require 'setup-clojure)
(require 'setup-coffeescript)
(require 'setup-css)
(require 'setup-cucumber)
(require 'setup-html)
(require 'setup-java)
(require 'setup-javascript)
(require 'setup-ledger)
(require 'setup-lisp)
(require 'setup-ruby)
(require 'setup-xml)
(ensure-package-and-require 'lua-mode)
(ensure-package-and-require 'markdown-mode)
(ensure-package-and-require 'puppet-mode)
(ensure-package-and-require 'yaml-mode)

;; Associate extensions with their specific modes
(require 'setup-mode-mappings)

;; Setup global key-bindings
(require 'setup-global-bindings)

;; Make emacs more beautiful
(require 'setup-appearance)
