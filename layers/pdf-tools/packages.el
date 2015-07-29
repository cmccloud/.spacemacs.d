;;; packages.el --- pdf-tools Layer packages File for Spacemacs
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
(setq pdf-tools-packages
    '(
      pdf-tools
      popwin
      eyebrowse
      persp-mode
      ace-jump-mode
      ))

;; List of packages to exclude.
(setq pdf-tools-excluded-packages '())

(defun pdf-tools/init-pdf-tools ()
  (use-package pdf-tools
    :preface
    (progn
      (defun pdf-tools/pdf-view-save-current-page-to-buffer (&optional args)
        "For any pdf-view windows, saves the current page to buffer.
PDF-Tools uses image-mode-winprops to track state. Other packages,
particularly those involving window management, sometimes delete windows
automatically. On restoration, the associated window-props are gone, and
pdf-view's default behavior is to pop to the first page. This function
introduces a fix for that behavior by saving the current page property
from the winprops-alist as a buffer local variable.

Also see pdf-tools/pdf-view-restore-current-page-from-buffer."
        (cl-loop for win in (window-list)
                 when (eql 'pdf-view-mode
                           (buffer-local-value 'major-mode (window-buffer win)))
                 do (with-selected-window win
                      (setq-local pdf-view-last-visited-page
                                  (pdf-view-current-page)))))

      (defun pdf-tools/pdf-view-restore-current-page-from-buffer (&optional args)
        "For any pdf-view windows, restores the current page from buffer.
PDF-Tools uses image-mode-winprops to track state. Other packages,
particularly those involving window management, sometimes delete windows
automatically. On restoration, the associated window-props are gone, and
pdf-view's default behavior is to pop to the first page. This function
introduces a fix for that behavior by restoring the page from a buffer
local backup.

Also see pdf-tools/pdf-view-save-current-page-to-buffer."
        (cl-loop for win in (window-list)
                 when (eql 'pdf-view-mode
                           (buffer-local-value 'major-mode (window-buffer win)))
                 do (with-selected-window win
                      (pdf-view-goto-page pdf-view-last-visited-page)))))
    :config
    (progn
      (pdf-tools-install)
      (spacemacs|evilify-map pdf-view-mode-map
        :mode pdf-view-mode
        :bindings
        (kbd "j") 'pdf-view-scroll-up-or-next-page
        (kbd "k") 'pdf-view-scroll-down-or-previous-page
        (kbd "h") 'pdf-view-next-page
        (kbd "l") 'pdf-view-previous-page
        (kbd "s") 'pdf-occur
        (kbd "?") 'pdf-tools-help
        (kbd "g") 'pdf-view-goto-page
        (kbd "m") 'pdf-view-midnight-minor-mode
        (kbd "p") 'pdf-view-fit-page-to-window
        (kbd "=") 'pdf-view-scale-reset
        (kbd "+") 'pdf-view-enlarge
        (kbd "-") 'pdf-view-shrink)
      (spacemacs|evilify-map pdf-occur-buffer-mode-map
        :mode pdf-occur-buffer-mode
        :bindings
        (kbd "RET") 'pdf-occur-goto-occurrence
        (kbd "v") 'pdf-occur-view-occurrence)
      (setq pdf-occur-prefer-string-search t
            pdf-view-continuous t))))

(defun pdf-tools/pre-init-popwin ()
  "Adds pdf-tools page saving/restoration support to popwin."
  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn
      (add-hook 'popwin:before-popup-hook
                #'pdf-tools/pdf-view-save-current-page-to-buffer)

      (add-hook 'popwin:after-popup-hook
                #'pdf-tools/pdf-view-restore-current-page-from-buffer))))

(defun pdf-tools/pre-init-persp-mode ()
  "Adds pdf-tools page saving/restoration support to perspectives."
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (advice-add 'persp-switch
                  :before
                  #'pdf-tools/pdf-view-save-current-page-to-buffer)
      (advice-add 'persp-switch
                  :after
                  #'pdf-tools/pdf-view-restore-current-page-from-buffer))))

(defun pdf-tools/pre-init-eyebrowse ()
  "Adds pdf-tools page saving/restoration support to eyebrowse."
  (spacemacs|use-package-add-hook eyebrowse
    :post-config
    (progn
      (add-hook 'eyebrowse-pre-window-switch-hook
                #'pdf-tools/pdf-view-save-current-page-to-buffer)
      (add-hook 'eyebrowse-post-window-switch-hook
                #'pdf-tools/pdf-view-restore-current-page-from-buffer))))

(defun pdf-tools/pre-init-ace-jump-mode ()
  "Removes active pdf-view windows from ace jump mode scope."
  (spacemacs|use-package-add-hook ace-jump-mode
    :post-config
    (progn
      (defun pdf-tools/filtered-ace-scope (visual-areas)
        "Filters ace-jump visual-areas, removing those belonging to pdf-view
windows."
        (--filter (let* ((buf (window-buffer (aj-visual-area-window it)))
                         (mmode (buffer-local-value 'major-mode buf)))
                    (not (eql 'pdf-view-mode mmode)))
                  visual-areas))
      (advice-add
       'ace-jump-list-visual-area
       :filter-return
       #'pdf-tools/filtered-ace-scope))))



;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

