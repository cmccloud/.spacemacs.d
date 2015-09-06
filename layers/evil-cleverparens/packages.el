;;; packages.el --- evil-cleverparens Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq evil-cleverparens-packages '(evil-cleverparens))

(defun evil-cleverparens/init-evil-cleverparens ()
  (use-package evil-cleverparens
    :diminish evil-cleverparens-mode " Ⓒ"
    :init
    (progn
      (evil-leader/set-key (kbd "tc") 'evil-cleverparens-mode)
      (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
      (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
      ;; Don't shadow evil snipe
      (add-hook 'evil-cleverparens-mode-hook
                (lambda () (when evil-snipe-mode
                             (evil-define-key 'normal evil-cleverparens-mode-map
                               "s" 'evil-snipe-s)
                             (evil-define-key 'normal evil-cleverparens-mode-map
                               "S" 'evil-snipe-S)))))))
