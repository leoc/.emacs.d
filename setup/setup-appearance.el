(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Recolor the region background
(set-face-background 'region "#464740")

;; Highlight current line
(global-hl-line-mode 1)

;; Customize background color of lighlighted line
(set-face-background 'hl-line "#222222")

(set-face-foreground 'font-lock-warning-face "#ff6666")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; No menu bars
(menu-bar-mode -1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (turn-off-tool-bar)
  (tooltip-mode -1)
  (turn-off-tool-bar)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; Ditch them scrollbars
(scroll-bar-mode -1)

;; Make zooming affect frame instead of buffers
(ensure-package-and-require 'zoom-frm)

;; use my custom theme
(load (expand-file-name "summered-theme.el" themes-dir))
(load-theme 'summered t)

;; Customize the mode line
(setq-default mode-line-format
  (list
   '(:eval (propertize " %b " 'face 'font-lock-type-face
                              'help-echo (buffer-file-name))) "("
   (propertize "%02l" 'face 'font-lock-type-face) ","
   (propertize "%02c" 'face 'font-lock-type-face) ") ["
   (propertize "%p" 'face 'font-lock-constant-face) "/"
   (propertize "%I" 'face 'font-lock-constant-face) "] ["
   '(:eval (propertize "%m" 'face 'font-lock-string-face
                       'help-echo buffer-file-coding-system)) "] ["
   '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
                       'face 'font-lock-preprocessor-face
                       'help-echo (concat "Buffer is in "
                                          (if overwrite-mode "overwrite" "insert") " mode")))
   '(:eval (when (buffer-modified-p)
             (concat ","  (propertize "Mod" 'face 'font-lock-warning-face
                                            'help-echo "Buffer has been modified"))))
   '(:eval (when buffer-read-only
             (concat ","  (propertize "RO" 'face 'font-lock-type-face
                                           'help-echo "Buffer is read-only"))))
   "] " "%M"))


;; shorten the eshell prompt
(setq eshell-prompt-function
      (function
       (lambda ()
         (concat (file-name-nondirectory (eshell/pwd))
                 (if (= (user-uid) 0) " # " " $ ")))))

;; Bring the background through after init ..
(defun transparency (value &optional frame)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (let ((frame (or frame
                   (selected-frame))))
    (set-frame-parameter frame 'alpha value)))

(add-hook 'after-make-frame-functions '(lambda (frame) (transparency 95 frame)))
(add-hook 'after-init-hook '(lambda () (transparency 95)))

(provide 'setup-appearance)
