;;; window-defuns.el --- Some window helpers
;;; Commentary:
;;; Code:

;; (defvar frame-winset-mode-line "")
;; (put 'frame-winset-mode-line 'risky-local-variable t)

;; (unless (memq 'frame-winset-mode-line global-mode-string)
;;   (setq global-mode-string (append '(frame-winset-mode-line)
;;                                    global-mode-string)))

;; (defface frame-winset-active-face
;;   '((t (:foreground "yellow" :bold 't)))
;;   "Winset mode line color"
;;   :group 'faces)

;; (defface frame-winset-inactive-face
;;   '((t (:foreground "gray")))
;;   "Winset mode line color"
;;   :group 'faces)

(defun frame-winset-update-mode-line ()
  "Set the modeline accordingly to the current state."
  (let ((current-index (frame-parameter nil 'window-configuration-index)))
    (setq frame-winset-mode-line
          (loop for element in '("[" 0 1 2 3 4 5 6 7 8 "]")
                collect (let ((element-string (format "%s " (if (stringp element)
                                                               element
                                                              (+ 1 element)))))
                          (if (eq element current-index)
                              (propertize element-string
                                          'face 'frame-winset-active-face)
                            (propertize element-string
                                        'face 'frame-winset-inactive-face)))))
  (force-mode-line-update)))

(defun window-toggle-maximize ()
  "Make the current window the maximum and go back."
  (interactive)
  (let ((last-window-configuration (frame-parameter nil 'temp-buffer-save)))
    (if last-window-configuration
        (progn
          (set-window-configuration last-window-configuration)
          (set-frame-parameter nil 'temp-buffer-save nil))
        (progn
          (set-frame-parameter nil 'temp-buffer-save (current-window-configuration))
          (delete-other-windows)))))

(defun substitute-nth (n value list)
  "Substitute the element at N by VALUE in given LIST."
  (loop for i from 0
        for j in list
        collect (if (= i n) value j)))

(set-frame-parameter nil 'window-configurations '(nil nil nil nil nil nil nil nil nil))
(set-frame-parameter nil 'window-configuration-index 0)

(defun window-setup-frame (frame)
  "Set the frame parameters of FRAME needed for fast window configuration switching."
  (set-frame-parameter frame 'window-configurations '(nil nil nil nil nil nil nil nil nil))
  (set-frame-parameter frame 'window-configuration-index 0))
(add-hook 'after-make-frame-functions 'window-setup-frame)

(defun window-switch-to-configuration (index)
  "Switch to a frame local window configuration with INDEX."
  (let* ((index (- index 1))
         (current-index (frame-parameter nil 'window-configuration-index))
         (configurations (frame-parameter nil 'window-configurations))
         (new-configurations (substitute-nth current-index (current-window-configuration) configurations)))
    (unless (eq index current-index)
      (set-frame-parameter nil 'window-configurations new-configurations)
      (set-frame-parameter nil 'window-configuration-index index)
      (if (nth index configurations)
          (set-window-configuration (nth index configurations))
        (delete-other-windows))
      (frame-winset-update-mode-line))))

(global-set-key (kbd "M-1") '(lambda () (interactive) (window-switch-to-configuration 1)))
(global-set-key (kbd "M-2") '(lambda () (interactive) (window-switch-to-configuration 2)))
(global-set-key (kbd "M-3") '(lambda () (interactive) (window-switch-to-configuration 3)))
(global-set-key (kbd "M-4") '(lambda () (interactive) (window-switch-to-configuration 4)))
(global-set-key (kbd "M-5") '(lambda () (interactive) (window-switch-to-configuration 5)))
(global-set-key (kbd "M-6") '(lambda () (interactive) (window-switch-to-configuration 6)))
(global-set-key (kbd "M-7") '(lambda () (interactive) (window-switch-to-configuration 7)))
(global-set-key (kbd "M-8") '(lambda () (interactive) (window-switch-to-configuration 8)))
(global-set-key (kbd "M-9") '(lambda () (interactive) (window-switch-to-configuration 9)))
(global-set-key (kbd "M-0") '(lambda () (interactive) (window-toggle-maximize)))

(provide 'window-defuns)

;;; window-defuns.el ends here
