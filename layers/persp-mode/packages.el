;;; packages.el --- persp-mode Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq persp-mode-packages '(persp-mode
                            helm))

(defun persp-mode/pre-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      (defun helm-perspectives ()
        "Selects or creates perspective."
        (interactive)
        (helm
         :buffer "*Helm Perspectives*"
         :sources `(,(helm-build-in-buffer-source
                         (s-concat "Current Perspective: "
                                   (safe-persp-name (get-frame-persp)))
                       :data (persp-names-sorted)
                       :fuzzy-match t
                       :action
                       '(("Switch to perspective" . persp-switch)))
                    ,(helm-build-dummy-source "Create new perspective"
                       :action
                       '(("Create new perspective" . persp-switch)))))))))

(defun persp-mode/init-persp-mode ()
  (use-package persp-mode
    :diminish persp-mode
    :preface
    (progn
      (defvar persp-mode-autosave t
        "If true, saves perspectives to file per `persp-autosave-interval'")
      (defvar persp-autosave-interval 900
        "Delay in seconds between `persp-autosave'.")
      (defvar persp-autosave-timer nil
        "Stores `persp-autosave' for removal on exit.")
      (defun persp-autosave ()
        "Perspectives mode autosave.
Autosaves perspectives layouts every `persp-autosave-interal' seconds.
Cancels autosave on exiting perspectives mode."
        (message "Perspectives mode autosaving enabled.")
        (if persp-mode
            (setq persp-autosave-timer
                  (run-with-timer
                   persp-autosave-interval
                   persp-autosave-interval
                   (lambda ()
                     (message "Saving perspectives to file.")
                     (persp-save-state-to-file))))
          (when persp-autosave-timer
            (cancel-timer persp-autosave-timer)
            (setq persp-autosave-timer nil))))
      (defun spacemacs/persp-number ()
        "Returns index of current perspective in `PERSP-NAMES-SORTED'."
        (let ((persp-num (-elem-index (safe-persp-name (get-frame-persp))
                                      (persp-names-sorted))))
          (cond ((= 0 persp-num) "⓪")
                ((= 1 persp-num) "①")
                ((= 2 persp-num) "②")
                ((= 3 persp-num) "③")
                ((= 4 persp-num) "④")
                ((= 5 persp-num) "⑤")
                ((= 6 persp-num) "⑥")
                (t persp-num)))))
    :config
    (progn
      (setq persp-nil-name "@spacemacs")
      (spacemacs/declare-prefix "L" "layouts")
      (evil-leader/set-key
        "Ls" #'persp-switch
        "Lr" #'persp-rename
        "Lc" #'persp-kill
        "La" #'persp-add-buffer
        "Lt" #'persp-temporarily-display-buffer
        "Lk" #'persp-remove-buffer)
      (when persp-mode-autosave
        (add-hook 'persp-mode-hook #'persp-autosave))

      (spacemacs|define-mode-line-segment persp-number
        (spacemacs/persp-number)
        :when (bound-and-true-p persp-mode))

      (setq spacemacs-mode-line-left
            (cons '((persp-number window-number)
                    :fallback state-tag
                    :seperator "|"
                    :face state-face)
                  (cdr spacemacs-mode-line-left)))
      (persp-mode t))))


