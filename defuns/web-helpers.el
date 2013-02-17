(defun search-google ()
  "Googles a query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Google: "))))))

(defun search-dict ()
  "Looks up in a dictionary. Query or region if any."
  (interactive)
  (browse-url
   (concat
    "http://dict.leo.org/ende?lp=ende&search="
    (url-hexify-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (read-string "Dict: "))))))
