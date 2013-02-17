(load "~/.ercpass")
(require 'erc-services)
(erc-services-mode 1)

(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-passwords
      `((freenode     (("leoc" . ,freenode-pass)))))

(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net"
         "#archlinux"
         "#clojure"
         "#clojurescript"
         "#emacs"
         "#ledger"
         "#lisp"
         "#maven"
         "#openzwave"
         "#org-mode"
         "#overtone"
         "#raspberrypi"
         "#sbcl"
         "#wayland"
         )
        ("irc.codehaus.org"
         "#maven")))

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(erc-button-mode nil)

(setq erc-user-full-name "Arthur Andersen")
(setq erc-email-userid "leoc.git@gmail.com")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))


(defun erc-privmsg-notify (proc res)
  (flet ((rtrim-string (s) (replace-regexp-in-string "\\([[:space:]\n]*$\\)" "" s)))
    (let ((channel-buffers     (erc-channel-list proc))
          (sender              (or (car (split-string (erc-response.sender res) "!"))
                                   (erc-response.sender res)))
          (target-channel-name (car (erc-response.command-args res)))
          (xwindow-class       (rtrim-string (shell-command-to-string "stumpish current-window-class"))))
      (unless (or (string= xwindow-class "Emacs") ; we are in an emacs frame
                  (member (get-buffer target-channel-name) channel-buffers)) ; this is a channel message
        (progn (notify "Instant message!"
                       (format "Direct message from %s" sender)
                       :timeout  120000
                       :app "ERC")
               nil)))))
(add-hook 'erc-server-PRIVMSG-functions 'erc-privmsg-notify)


;; Color Nicks
(defmacro unpack-color (color red green blue &rest body)
  `(let ((,red   (car ,color))
         (,green (car (cdr ,color)))
         (,blue  (car (cdr (cdr ,color)))))
     ,@body))

(defun rgb-to-html (color)
  (unpack-color color red green blue
   (concat "#" (format "%02x%02x%02x" red green blue))))

(defun hexcolor-luminance (color)
  (unpack-color color red green blue
   (floor (+ (* 0.299 red) (* 0.587 green) (* 0.114 blue)))))

(defun invert-color (color)
  (unpack-color color red green blue
   `(,(- 255 red) ,(- 255 green) ,(- 255 blue))))

(defun erc-get-color-for-nick (nick dark)
  (let* ((hash     (md5 (downcase nick)))
         (red      (mod (string-to-number (substring hash 0 10) 16) 256))
         (blue     (mod (string-to-number (substring hash 10 20) 16) 256))
         (green    (mod (string-to-number (substring hash 20 30) 16) 256))
         (color    `(,red ,green ,blue)))
    (rgb-to-html (if (if dark (< (hexcolor-luminance color) 85)
                       (> (hexcolor-luminance color) 170))
                     (invert-color color)
                   color))))

(defun erc-highlight-nicknames ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\w+" nil t)
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (nick   (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (when (erc-get-server-user nick)
          (put-text-property
           (car bounds) (cdr bounds) 'face
           (cons 'foreground-color (erc-get-color-for-nick nick 't))))))))

(add-hook 'erc-insert-modify-hook 'erc-highlight-nicknames)

(provide 'setup-irc)
