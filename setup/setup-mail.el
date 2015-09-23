;;; setup-mail.el --- Setup mu4e
;;; Commentary:
;;; Code:


(defvar my-mu4e-account-alist nil
  "Defines all mu4e accounts.")
(load "~/.mu4e-accounts.el")
(load "~/.mu4e-refile-assocs.el")

;; Require mu4e. Mu4e is installed on my archlinux system from AUR.
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(require 'smtpmail)

(setq send-mail-function 'smtpmail-send-it

      mu4e-get-mail-command (expand-file-name "~/.bin/force_mbsync -a")
      mu4e-update-interval 60

      mu4e-maildir "/home/arthur/.mail/"

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

      mu4e-bookmarks '(("flag:unread AND NOT flag:trashed"      "Unread messages"      ?u)
                       ("date:today..now AND NOT flag:trashed"  "Today's messages"     ?t)
                       ("date:7d..now AND NOT flag:trashed"     "Last 7 days"          ?w)
                       ("mime:image/*"                          "Messages with images" ?p))

      mu4e-headers-fields '((:maildir      . 30)
                            (:date         . 19)
                            (:flags        . 6)
                            (:from-or-to   . 28)
                            (:mailing-list . 20)
                            (:subject      . nil))

      ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
      mu4e-sent-messages-behavior 'delete
      mu4e-confirm-quit nil

      ;; Try to display images in mu4e
      mu4e-view-show-images t
      mu4e-view-image-max-width 800

      ;; use 'fancy' non-ascii characters in various places in mu4e
      mu4e-use-fancy-chars t

      ;; save attachment to my desktop (this can also be a function)
      mu4e-attachment-dir "~/Downloads"

      user-full-name "Arthur Leonard Andersen"
      mu4e-headers-date-format "%d.%b %Y %H:%M" ; date format

      mu4e-html2text-command 'my-html2text

      message-kill-buffer-on-exit t
      message-send-mail-function 'smtpmail-send-it
      mail-user-agent 'mu4e-user-agent)

(setq mu4e-headers-draft-mark     (purecopy '("D" . "D")))
(setq mu4e-headers-flagged-mark   (purecopy '("F" . "F")))
(setq mu4e-headers-new-mark       (purecopy '("N" . "N")))
(setq mu4e-headers-passed-mark    (purecopy '("P" . "P")))
(setq mu4e-headers-replied-mark   (purecopy '("R" . "R")))
(setq mu4e-headers-seen-mark      (purecopy '("S" . "S")))
(setq mu4e-headers-trashed-mark   (purecopy '("T" . "T")))
(setq mu4e-headers-attach-mark    (purecopy '("a" . "a")))
(setq mu4e-headers-encrypted-mark (purecopy '("x" . "x")))
(setq mu4e-headers-signed-mark    (purecopy '("s" . "s")))
(setq mu4e-headers-unread-mark    (purecopy '("u" . "u")))

;; thread prefix marks
(setq mu4e-headers-has-child-prefix    (purecopy '("+"  . "└┬")))
(setq mu4e-headers-empty-parent-prefix (purecopy '("-"  . "─")))
(setq mu4e-headers-first-child-prefix  (purecopy '("\\" . "├")))
(setq mu4e-headers-duplicate-prefix    (purecopy '("="  . "═")))
(setq mu4e-headers-default-prefix       (purecopy '("|"  . "├")))


(defun my-html2text ()
  "Replacement for standard html2text using shr."
  (interactive)
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

(load "~/.mu4e-refile-assocs.el")

;; Set custom faces

;; Smart refile locations
(setq mu4e-refile-folder
      '(lambda (msg)
        (let* ((maildir (mu4e-message-field msg :maildir))
               (account (my-mu4e-find-account 'mu4e-maildir-prefix maildir))
               (maildir-prefix (my-mu4e-account-value account 'mu4e-maildir-prefix))
               (maildir-postfix (catch 'found
                                  (dolist (assoc my-mu4e-refile-assocs)
                                    (let ((postfix (car assoc))
                                          (sender-list (cdr assoc)))
                                      (dolist (sender sender-list)
                                        (when (mu4e-message-contact-field-matches msg :from sender)
                                          (throw 'found postfix))))))))
          (if maildir-postfix
              (concat maildir-prefix "/" maildir-postfix)
            (concat maildir-prefix "/Archive")))))

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
      message ""
      smtpmail-smtp-service 587)

(defvar my-mu4e-account-alist nil
  "Defines all mu4e accounts.")
(load "~/.mu4e-accounts.el")

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

(defun my-mu4e-account-value (account var)
  "Find the value for a given ACCOUNT VAR."
  (let ((account-vars (cdr (assoc account my-mu4e-account-alist)))
        value)
    (if account-vars
        (mapc #'(lambda (pair)
                  (if (eq (car pair) var)
                      (setq value (cadr pair))))
              account-vars))
    value))

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
  (when msg
    (let* ((maildir (mu4e-message-field msg :maildir))
           (account (my-mu4e-find-account 'mu4e-maildir-prefix maildir)))
      (my-mu4e-set-account-variables account))))

(ad-activate 'mu4e~get-folder)

(defun my-mu4e-set-account-for-composition ()
  "Ask for and set the account to compose a new message with."
  (let* ((account (if mu4e-compose-parent-message
                      (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                        (my-mu4e-find-account 'mu4e-maildir-prefix maildir))
                    (completing-read (format "Compose with account: (%s) "
                                             (mapconcat #'(lambda (var) (car var)) my-mu4e-account-alist "/"))
                                     (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                                     nil t nil nil (caar my-mu4e-account-alist)))))
    (my-mu4e-set-account-variables account)))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account-for-composition)

(provide 'setup-mail)
;;; setup-mail.el ends here
