;;; setup-irc.el --- Setup IRC.

;;; Commentary:
;; Setup erc-znc.el.  Most of the channel settings are configured
;; within znc on my private server.

;;; Code:
(require 'erc-services)
(erc-services-mode 1)

;; Load credentials...
(load "~/.ercpass")

;; Configure ZNC
(ensure-package 'znc)
(require 'znc)

(setq znc-servers `(("arthur.vela" 8887 t ((freenode ,erc-freenode-user ,erc-freenode-pass)
                                           (oftc ,erc-oftc-user ,erc-oftc-pass)))))

;; Show timestamps!
(erc-timestamp-mode t)

;; Hide certain stuff...
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(provide 'setup-irc)

;;; setup-irc.el ends here
