(defun irc-freenode ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
       :nick "leoc" :full-name "Arthur Andersen"))

(defun irc-codehaus ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.codehaus.org" :port 6667
       :nick "leoc" :full-name "Arthur Andersen"))
