(require 'cl)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq url-http-attempt-keepalives nil)

(defvar custom-packages
  '(ac-slime
    ace-jump-mode
    ack-and-a-half
    auto-complete
    auto-complete-clang
    clojure-mode
    coffee-mode
    dired-details
    expand-region
    elisp-slime-nav
    find-file-in-project
    fill-column-indicator
    flycheck
    gist
    git-commit-mode
    gitconfig-mode
    gitignore-mode
    haml-mode
    haskell-mode
    helm
    helm-projectile
    key-chord
    inf-ruby
    ibuffer-vc
    less-css-mode
    lua-mode
    magit
    markdown-mode
    multiple-cursors
    melpa
    paredit
    popup
    rainbow-mode
    revive
    ruby-block
    ruby-end
    ruby-tools
    rvm
    sass-mode
    scss-mode
    smex
    volatile-highlights
    wgrep
    yaml-mode
    yasnippet
    zoom-frm)
  "A list of packages to ensure are installed at launch.")

(setq custom-packages
      '(ac-slime
        ace-jump-mode
        ack-and-a-half
        auto-complete
        auto-complete-clang
        browse-kill-ring
        bash-completion
        clojure-mode
        coffee-mode
        dired-details
        expand-region
        elisp-slime-nav
        eproject
        ess
        emacs-eclim
        find-file-in-project
        fill-column-indicator
        flycheck
        frame-cmds
        frame-fns
        gist
        git-commit-mode
        gitconfig-mode
        gitignore-mode
        haml-mode
        haskell-mode
        helm
        helm-projectile
        key-chord
        inf-ruby
        ido-ubiquitous
        ibuffer-vc
        less-css-mode
        lua-mode
        magit
        markdown-mode
        multiple-cursors
        markup
        melpa
        paredit
        popup
        rainbow-mode
        revive
        ruby-block
        ruby-end
        ruby-tools
        rvm
        sass-mode
        scss-mode
        undo-tree
        smooth-scrolling
        smex
        smart-forward
        shell-command
        volatile-highlights
        wgrep
        yaml-mode
        yasnippet
        zoom-frm))

(defun custom-packages-installed-p ()
  (loop for p in custom-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (custom-packages-installed-p)
  (package-refresh-contents)
  (dolist (p custom-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'setup-packages)
