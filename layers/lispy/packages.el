;;; packages.el --- lispy Layer packages File for Spacemacs
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
(setq lispy-packages
    '(
      lispy
      evil))

;; List of packages to exclude.
(setq lispy-excluded-packages '())

(defun lispy/init-lispy ()
  (use-package lispy
    :diminish lispy-mode " Ⓛ"
    :preface
    (progn
      (defvar lispy-major-modes '(emacs-lisp-mode clojure-mode)
        "List of major modes associated with lispy.
Used by `lispy-enter-maybe'.")
      (defun lispy-enter-maybe ()
        "Enters lispy-mode if in a major mode listed in `lispy-major-modes'."
        (when (--some? (eql it major-mode) lispy-major-modes)
          (lispy-mode)))
      (defun lispy-exit ()
        "Exits lispy mode"
        (lispy-mode -1))
      (defun lispy-toggle ()
        "Toggles lispy mode."
        (interactive)
        (if lispy-mode (lispy-mode -1) (lispy-mode))))
    :config
    (progn
      ;; lispy keybindings
      (define-key evil-emacs-state-map (kbd "C-c l") 'lispy-toggle)
      (define-key lispy-mode-map (kbd "C-c l") 'lispy-toggle)
      (define-key lispy-mode-map (kbd "C-c C-f") 'helm-multi-files)
      (define-key lispy-mode-map (kbd "C-?") 'helm-descbinds)
      (define-key lispy-mode-map (kbd "C-o") 'evil-jumper/backward)
      (define-key lispy-mode-map (kbd "TAB") 'evil-jumper/forward)
      (define-key lispy-mode-map (kbd "C-f") 'lispy-forward)
      (define-key lispy-mode-map (kbd "C-d") 'lispy-backward)
      (define-key lispy-mode-map (kbd "M-u") 'lispy-undo)
      (define-key lispy-mode-map (kbd "[") 'lispy-brackets)
      (define-key lispy-mode-map (kbd "{") 'lispy-braces)
      ;; use helm rather than ivy
      (advice-add 'lispy-occur :override #'helm-occur))))

(defun lispy/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config
    (progn
      (add-hook 'evil-emacs-state-entry-hook #'lispy-enter-maybe)
      (add-hook 'evil-emacs-state-exit-hook #'lispy-exit))))
