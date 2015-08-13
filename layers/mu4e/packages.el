;;; packages.el --- mu4e Layer packages File for Spacemacs
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
(setq mu4e-packages
      '(
        helm
        ))

(defun mu4e/post-init-helm ()
  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      (defun helm-mu4e-completing-read (prompt collection &optional
                                               predicate
                                               require-match
                                               initial-input
                                               hist
                                               def
                                               inherit-input-method)
        "Helm completing read for mu4e with fuzzy matching."
        (helm-comp-read prompt collection
                        :buffer "*helm-mu4e-completing-read*"
                        :name "Mail Directories"
                        :initial-input initial-input
                        :must-match require-match
                        :sort predicate
                        :history hist
                        :default def
                        :fuzzy t))

      (setq mu4e-completing-read-function #'helm-mu4e-completing-read))))
