(defun open-in-external-app ()
  "Open the current file or dired marked files in external app."
  (interactive)
  (let* ((file-list (dired-get-marked-files))
         (proceed-p (if (<= (length file-list) 5)
                        t
                      (y-or-n-p "Open more than 5 files?"))))
    (when proceed-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (file-path)
                (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" file-path t t)))
              file-list))
       ((string-equal system-type "darwin")
        (mapc (lambda (file-path)
                (shell-command (format "open \"%s\"" file-path)))
              file-list))
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (file-path)
                (let (process-connection-type)
                  (start-process "" nil "xdg-open" file-path)))
              file-list))))))
