;;; packages.el --- lively Layer packages File for Spacemacs
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

(setq lively-packages '(lively))

(defun lively/init-lively ()
  (use-package lively
    :commands (lively lively-stop)
    :init
    (progn
      (evil-leader/set-key-for-mode
        'emacs-lisp-mode
        "msl" 'lively
        "mss" 'lively-stop))))
