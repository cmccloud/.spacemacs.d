;;; extensions.el --- mu4e Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Christopher McCloud <mccloud.christopher@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mu4e-post-extensions '(mu4e))

(defun mu4e/init-mu4e ()
  (use-package mu4e
    :load-path "/usr/local/share/emacs/site-lisp/mu4e"
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (evil-leader/set-key "am" 'mu4e)
      (global-set-key (kbd "C-c m") 'mu4e-compose-new))
    :config
    (progn
      (require 'mu4e-contrib)
      ;; keybindings
      (evilify mu4e-main-mode mu4e-main-mode-map
               "j" 'mu4e~headers-jump-to-maildir)
      (evilify mu4e-headers-mode mu4e-headers-mode-map)
      (evilify mu4e-view-mode mu4e-view-mode-map
               "J" 'mu4e-view-headers-next
               "K" 'mu4e-view-headers-prev)

      ;; mailbox shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)))

      ;; headers
      (setq mu4e-headers-fields
            '((:human-date . 12)
              (:flags . 6)
              (:from . 22)
              (:subject)))

      ;; bookmarks
      (setq mu4e-bookmarks
            '(("flag:flagged AND NOT flag:trashed"
               "Flagged Messages" ?f)
              ("flag:unread AND date:7d..now AND NOT flag:trashed"
               "Latest Unread Messages" ?u)
              ("date:7d..now AND NOT flag:trashed"
               "Latest Messages" ?l)
              ("flag:unread AND NOT flag:trashed"
               "All Unread Messages" ?a)
              ("date:today..now AND NOT flag:trashed"
               "Today's messages" ?t)))

      ;; include signature
      (setq mu4e-compose-signature
            (concat "Christopher McCloud\n"
                    "mccloud.christopher@gmail.com\n")
            mu4e-compose-signature-auto-include t)

      (setq mu4e-get-mail-command "offlineimap -q"
            mu4e-attachment-dir "~/Downloads"
            mu4e-update-interval 300
            mu4e-confirm-quit nil
            mu4e-completing-read-function 'helm--completing-read-default
            mu4e-view-show-images t
            mu4e-view-prefer-html t
            mu4e-compose-dont-reply-to-self t
            mu4e-hide-index-messages t
            mu4e-compose-complete-only-personal nil
            mu4e-html2text-command 'mu4e-shr2text
            mu4e-headers-skip-duplicates t
            mu4e-view-show-addresses t
            mu4e-sent-messages-behavior 'delete
            message-kill-buffer-on-exit t)

      ;; maildirs
      (setq mu4e-maildir "~/.mail/gmail"
            mu4e-drafts-folder "/[Gmail].Drafts"
            mu4e-sent-folder "/[Gmail].Sent Mail"
            mu4e-trash-folder "/[Gmail].Trash")

      ;; sending mail
      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587))))
