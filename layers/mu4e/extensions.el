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

      ;; mailbox shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)))

      ;; headers
      (setq mu4e-headers-fields
            '((:human-date . 12)
              (:tags . 20)
              (:flags . 6)
              (:from . 22)
              (:subject)))

      ;; gmail:: archive and mark as read
      (add-to-list 'mu4e-marks
                   '(archive
                     :char "A"
                     :prompt "Archive"
                     :show-target (lambda (target) "archive")
                     :action (lambda (docid msg target)
                               (mu4e-action-retag-message msg "-\\Inbox")
                               (mu4e~proc-move docid nil "+S-u-N"))))
      (mu4e~headers-defun-mark-for archive)

      ;; adding tags
      (add-to-list 'mu4e-marks
                   '(tag
                     :char "g"
                     :prompt "gtag"
                     :ask-target (lambda () (read-string "Tag Name: "))
                     :action (lambda (docid msg target)
                               (mu4e-action-retag-message msg (concat "+" target)))))
      (mu4e~headers-defun-mark-for tag)

      ;; bookmarks
      (setq mu4e-bookmarks
            `((,(s-join " AND NOT "
                        '("flag:unread"
                          "tag:updates"
                          "tag:social"
                          "tag:promotions"
                          "tag:forums"))
               "Inbox" ?n)
              ("flag:flagged"
               "Flagged Messages" ?i)
              ("flag:unread AND tag:updates"
               "Updates" ?u)
              ("flag:unread AND tag:social"
               "Social" ?s)
              ("flag:unread AND tag:promotions"
               "Promotions" ?p)
              ("flag:unread AND tag:forums"
               "Forums" ?f)
              ("flag:unread AND NOT flag:trashed"
               "Unread Messages" ?a)
              ("date:today..now AND NOT flag:trashed"
               "Today's messages" ?t)
              ("date:7d..now AND NOT flag:trashed"
               "Week's Messages" ?l)
              ("tag:finance"
               "All by tag: Finance" ?F)
              ("tag:hr"
               "All by tag: Hack Reactor" ?H)
              ("tag:programming"
               "All by tag: Programming" ?P)))

     ;; keybindings
      (evilify mu4e-main-mode mu4e-main-mode-map
               "j" 'mu4e~headers-jump-to-maildir)
      (evilify mu4e-headers-mode mu4e-headers-mode-map
               "J" 'mu4e-headers-next
               "K" 'mu4e-headers-prev
               "a" 'mu4e-headers-action
               "A" 'mu4e-headers-mark-for-archive
               "g" 'mu4e-headers-mark-for-tag)
      (evilify mu4e-view-mode mu4e-view-mode-map
               "J" 'mu4e-view-headers-next
               "K" 'mu4e-view-headers-prev)

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
