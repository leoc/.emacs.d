(require 'dash)
(require 'dired)
(require 'dired-x)

(setq dired-to-archive-copy-alist
      '(("\\.sh\\(ar\\|[0-9]\\)*$" nil "shar %s > %s")
        ("\\.jar$" ("jar" "uvf") ("jar" "cvf"))
        ("\\.tar$" ("tar" "-uf") ("tar" "-cf"))
        ("\\.tgz$\\|\\.tar\\.g?[zZ]$" ("tar" "-uf %s" "|" "gzip > %s") ("tar" "-czvf"))
        ("\\.ear$" ("zip" "-qr") ("zip" "-qr"))
        ("\\.war$" ("zip" "-qr") ("zip" "-qr"))
        ("\\.zip$" ("zip" "-qr") ("zip" "-qr"))
        ("\\.wmz$" ("zip" "-qr") ("zip" "-qr"))
        ("\\.arc$" ("arc" "a") nil)
        ("\\.zoo$" ("zoo" "aP") nil)))

(setq archive-zip-use-pkzip nil)
(add-to-list 'auto-mode-alist '("\\.[ejrw]ar$\\'" . archive-mode))

(defconst MY_TRYOUT_DIR "~/downloads/tryout"
  "Directory for extracting files")

(setq dired-extract-alist
      `(("\\.u\\(ue\\|aa\\)$" . dired-uud)
        ("\\.jar$" . "jar -xvf %s")
        ("\\.tar$" . ,(concat "tar -xf %s -C " MY_TRYOUT_DIR))
        ("\\.tgz$\\|\\.tar\\.g?[zZ]$" . ,(concat "tar -xzf %s -C " MY_TRYOUT_DIR))
        ("\\.arc$" . "arc x %s ")
        ("\\.bz2$" . ,(concat "bunzip2 -q %s"))
        ("\\.rar$" . ,(concat "unrar x %s " MY_TRYOUT_DIR "\\"))
        ("\\.zip$" . ,(concat "unzip -qq -Ux %s -d " MY_TRYOUT_DIR))
        ("\\.ear$" . ,(concat "unzip -qq -Ux %s -d " MY_TRYOUT_DIR))
        ("\\.war$" . ,(concat "unzip -qq -Ux %s -d " MY_TRYOUT_DIR))
        ("\\.zoo$" . "zoo x. %s ")
        ("\\.lzh$" . "lha x %s ")
        ("\\.7z$"  . "7z e %s ")
        ("\\.g?[zZ]$" . "gzip -d %s")))

;; Make dired less verbose
(ensure-package-and-require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

(add-hook 'dired-mode-hook '(lambda ()
                              (diff-hl-dired-mode)))

;; Reload dired after making changes
(--each '(dired-do-rename
          dired-create-directory
          wdired-abort-changes)
        (eval `(defadvice ,it (after revert-buffer activate)
                 (revert-buffer))))

(setq dired-omit-files "^\\.[^.].*$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

;; Delete with C-x C-k to match file buffers and magit
(define-key dired-mode-map (kbd "C-x C-k") 'dired-do-delete)
(define-key dired-mode-map (kbd "C-o") 'open-in-external-app)
(define-key dired-mode-map (kbd "C-c C-o") 'dired-omit-mode)

(provide 'setup-dired)
