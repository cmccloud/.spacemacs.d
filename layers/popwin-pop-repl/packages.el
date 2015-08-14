;;; packages.el --- popwin-pop-repl Layer packages File for Spacemacs
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
(setq popwin-pop-repl-packages
    '(
      popwin
      ))

;; List of packages to exclude.
(setq popwin-pop-repl-excluded-packages '())

(defun popwin-pop-repl/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn
      (defvar popwin--pop-repl-table (make-hash-table)
        "Associates major modes to repl enviroments.")

      ;; generic command
      (defun popwin-pop-repl ()
        "Calls for major-mode repl using `popwin--pop-repl-table'."
        (interactive)
        (funcall (gethash major-mode popwin--pop-repl-table (lambda ()))))

      ;; cider repl
      (defun popwin--pop-cider-repl ()
        "Toggles as a popwin window the first active instance of cider repl."
        (when (get-buffer "*cider-repl localhost*")
          (if (get-buffer-window "*cider-repl localhost*" t)
              (progn
                (popwin:close-popup-window t)
                ;; only if popwin failed
                (-when-let (retry
                            (get-buffer-window "*cider-repl localhost*" t))
                  (delete-window retry)))
            (popwin:popup-buffer
             "*cider-repl localhost*"
             :position 'bottom
             :height .3
             :stick t
             :tail t
             :noselect t
             :dedicated t))))

      ;; ielm repl
      (defun popwin--pop-ielm-repl ()
        "Displays ielm popup buffer. Uses existing buffer if found."
        (let ((instance (get-buffer "*ielm*"))
              (options '("*ielm*"
                         :position bottom
                         :height .4
                         :stick t
                         :tail t
                         :noselect nil
                         :dedicated t))
              (buf (current-buffer)))
          (cond ((null instance)
                 (ielm)
                 (switch-to-buffer buf)
                 (apply 'popwin:popup-buffer options))
                ((get-buffer-window "*ielm*" t)
                 (popwin:close-popup-window t)
                 ;; only if popwin failed
                 (-when-let (retry (get-buffer-window "*ielm*" t))
                   (delete-window retry)))
                (t (apply 'popwin:popup-buffer options)))))

      ;; Associations
      (puthash 'inferior-emacs-lisp-mode
               #'popwin--pop-ielm-repl popwin--pop-repl-table)
      (puthash 'emacs-lisp-mode
               #'popwin--pop-ielm-repl popwin--pop-repl-table)
      (puthash 'clojure-mode
               #'popwin--pop-cider-repl popwin--pop-repl-table)
      (puthash 'cider-repl-mode
               #'popwin--pop-cider-repl popwin--pop-repl-table))))
