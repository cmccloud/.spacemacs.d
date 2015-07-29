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
(setq persp-mode-packages
    '(
      persp-mode
      helm
      ))

;; List of packages to exclude.
(setq persp-mode-excluded-packages '())

(defun persp-mode/init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      (defun helm-perspectives ()
        "Selects or creates perspective."
        (interactive)
        (helm :buffer "*Helm Perspectives*"
              :sources `(,(helm-build-in-buffer-source "Perspectives"
                            :data (persp-names-sorted)
                            :fuzzy-match t
                            :action '(("Switch to perspective" . persp-switch)))
                         ,(helm-build-dummy-source "Create new perspective"
                            :action '(("Create new perspective" . persp-switch)))))))))

(defun persp-mode/init-persp-mode ()
  (use-package persp-mode
    :preface
    (progn
      (defvar persp-mode-autosave t
        "If non-nil, saves state to file every persp-autosave-interval seconds")
      (defvar persp-autosave-interval 900
        "Delay in seconds between autosaves.")
      (defvar persp-autosave-timer nil
        "Stores persp-autosave for removal on exit."))
    :config
    (progn
      (defun persp-autosave ()
        (message "Perspectives mode autosaving enabled.")
        (if persp-mode
            (setq persp-autosave-timer
                  (run-with-timer
                   persp-autosave-interval
                   persp-autosave-interval
                   (lambda ()
                     (message "Saving perspectives to file.")
                     (persp-save-state-to-file))))
          (cancel-timer persp-autosave-timer)))
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
      (persp-mode t))))


;; For each package, define a function persp-mode/init-<package-name>
;;
;; (defun persp-mode/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
