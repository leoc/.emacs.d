(setq visible-bell t
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

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
(add-to-list 'load-path (expand-file-name "summered-theme" vendor-dir))
(load (expand-file-name "summered-theme/summered-theme.el" vendor-dir))
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

(defun set-frame-transparency (&optional frame)
  (transparency 98 frame))

(add-hook 'after-make-frame-functions 'set-frame-transparency)
(add-hook 'after-init-hook 'set-frame-transparency)

(provide 'setup-appearance)
