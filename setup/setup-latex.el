;; AUCTeX configuration
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(setq-default TeX-master nil)

;; use pdflatex
(setq TeX-PDF-mode t)

(setq TeX-view-program-selection
      '((output-dvi "DVI Viewer")
        (output-pdf "PDF Viewer")
        (output-html "HTML Viewer")))

(setq TeX-view-program-list
      '(("DVI Viewer" "xdg-open %o")
        ("PDF Viewer" "xdg-open %o")
        ("HTML Viewer" "xdg-open %o")))

(add-hook 'LaTeX-mode-hook '(lambda ()
                              (turn-on-auto-fill)
                              (abbrev-mode +1)))

(provide 'setup-latex)
