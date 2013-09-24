;;; setup-meil.el --- Setup mu4e

;;; Commentary:

;;; Code:
(require 'mu4e)
(require 'smtpmail)

(setq mu4e-get-mail-command "force_mbsync -a"
      mu4e-update-interval 60
      mu4e-maildir "~/.mail/"
      mu4e-my-email-addresses '("a.andersen@me.com"
                                "leoc.git@gmail.com"
                                "arthur@beloved-king.org")
      mu4e-use-fancy-chars t
      mu4e-maildir-shortcuts '( ("/gmail/Inbox"          . ?s)
                                ("/icloud/Inbox"         . ?d)
                                ("/tuberlin/Inbox"       . ?f)
                                ("/gmail/Sent"           . ?w)
                                ("/icloud/Sent"          . ?e)
                                ("/tuberlin/Sent"        . ?r)
                                ("/gmail/Trash"          . ?x)
                                ("/icloud/Trash"         . ?c)
                                ("/tuberlin/Trash"       . ?v))

      mu4e-headers-fields '((:date    . 25)
                            (:flags   .  6)
                            (:from    . 28)
                            (:subject . nil))

      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
      mu4e-sent-messages-behavior 'delete
      mu4e-confirm-quit nil

      ;; Try to display images in mu4e
      mu4e-view-show-images t
      mu4e-view-image-max-width 800

      ;; use 'fancy' non-ascii characters in various places in mu4e
      mu4e-use-fancy-chars nil

      ;; save attachment to my desktop (this can also be a function)
      mu4e-attachment-dir "~/Downloads"

      user-full-name "Arthur Leonard Andersen"
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format

      mu4e-html2text-command "pandoc --columns=79 -f html -t org"

      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      mail-user-agent 'mu4e-user-agent)

;; Smart refile locations
(setq mu4e-refile-folder
      (lambda (msg)
        (let* ((maildir (mu4e-message-field msg :maildir))
               (account (my-mu4e-find-account 'mu4e-maildir-prefix maildir)))
          (cond
           ((equal account "Gmail")
            (cond
             ((mu4e-message-contact-field-matches msg :from "uploaded.net") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "Host Europe - Rechnung") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "Sony Entertainment Network") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "service@hhv.de") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "momox") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "reBuy.de") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "buchankauf24") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "ShipRise Media") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "astrilda.de") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "cybertix.eu") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "[tT]+hreadless") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "[cC]+ashfix") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "paypal") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "Easy-Ankauf") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "deutschepost.de") "/gmail/Buy and Sell")
             ((mu4e-message-contact-field-matches msg :from "bonavendi") "/gmail/Buy and Sell")
             (t "/gmail/Archive")))
           ((equal account "iCloud")
            (cond (t "/icloud/Archive")))
           ((equal account "TU Berlin")
            (cond (t "/tuberlin/Archive")))
           ((equal account "Beloved King")
            (cond (t "/arthurorg/Archive")))
           (t "/Archive")))))

(setq mu4e-maildir-prefix "/gmail"
      mu4e-sent-folder "/Sent"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder  "/Trash"
      user-mail-address "leoc.git@gmail.com"
      message-signature-file ".gmail_signature.txt"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-local-domain "gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      starttls-use-gnutls t
      smtpmail-smtp-service 587)

(defvar my-mu4e-account-alist
  '(("Gmail"
     (mu4e-maildir-prefix "/gmail")
     (mu4e-sent-folder "/gmail/Sent Messages")
     (mu4e-drafts-folder "/gmail/Drafts")
     (mu4e-trash-folder  "/gmail/Trash")
     (user-mail-address "leoc.git@gmail.com")
     (message-signature-file ".gmail_signature.txt")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (starttls-use-gnutls t)
     (smtpmail-smtp-service 587))
    ("iCloud"
     (mu4e-maildir-prefix "/icloud")
     (mu4e-sent-folder "/icloud/Sent")
     (mu4e-drafts-folder "/icloud/Drafts")
     (mu4e-trash-folder  "/icloud/Trash")
     (user-mail-address "a.andersen@me.com")
     (message-signature-file ".icloud_signature.txt")
     (smtpmail-default-smtp-server "smtp.mail.me.com")
     (smtpmail-local-domain "me.com")
     (smtpmail-smtp-server "smtp.mail.me.com")
     (starttls-use-gnutls t)
     (smtpmail-smtp-service 587))
    ("TU Berlin"
     (mu4e-maildir-prefix "/tuberlin")
     (mu4e-sent-folder "/tuberlin/Sent")
     (mu4e-drafts-folder "/tuberlin/Drafts")
     (mu4e-trash-folder  "/tuberlin/Trash")
     (user-mail-address "a.andersen@mailbox.tu-berlin.de")
     (message-signature-file ".tuberlin_signature.txt")
     (smtpmail-default-smtp-server "mail.tu-berlin.de")
     (smtpmail-local-domain "tu-berlin.de")
     (smtpmail-smtp-server "mail.tu-berlin.de")
     (starttls-use-gnutls t)
     (smtpmail-smtp-service 587))
    ("Beloved King"
     (mu4e-maildir-prefix "/arthurorg")
     (mu4e-sent-folder "/arthurorg/INBOX.Sent")
     (mu4e-drafts-folder "/arthurorg/INBOX.Drafts")
     (mu4e-trash-folder  "/arthurorg/INBOX.Trash")
     (user-mail-address "arthur@beloved-king.org")
     (message-signature-file ".arthurorg_signature.txt")
     (smtpmail-default-smtp-server "smtp.art-hur.org")
     (smtpmail-local-domain "beloved-king.org")
     (smtpmail-smtp-server "smtp.art-hur.org")
     (starttls-use-gnutls t)
     (smtpmail-smtp-service 587))))

(defun my-mu4e-find-account (variable value)
  "Find the first account that match VARIABLE with VALUE.

The VALUE may be a sequence aswell, where the first account is returned,
which VARIABLE value is a member of the VALUE sequence."
  (car (find-if #'(lambda (account)
                    (let* ((account-vars (cdr account))
                           (variable-value (cadr (assoc variable account-vars))))
                      (cond ((listp value)
                             (member variable-value value))
                            ((eq variable 'mu4e-maildir-prefix)
                             (string-match variable-value value))
                            (t (equal variable-value value)))))
                my-mu4e-account-alist)))

(defun my-mu4e-set-account-variables (account)
  "Set the account variables for given email ACCOUNT."
  (let ((account-vars
         (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error (format "No email account found: %S" account)))))

(defadvice mu4e~get-folder (before load-corresponding-account (foldervar msg))
  (let* ((maildir (mu4e-message-field msg :maildir))
         (account (my-mu4e-find-account 'mu4e-maildir-prefix maildir)))
    (my-mu4e-set-account-variables account)))

;; (ad-update 'mu4e~get-folder)

(ad-activate 'mu4e~get-folder)

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account (if mu4e-compose-parent-message
                      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                        (my-mu4e-find-account 'mu4e-maildir-prefix maildir))
                    (completing-read (format "Compose with account: (%s) "
                                             (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                                     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                     nil t nil nil (caar my-mu4e-account-alist)))))
    (my-mu4e-set-account-variables account)))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(provide 'setup-mail)
;;; setup-mail.el ends here
