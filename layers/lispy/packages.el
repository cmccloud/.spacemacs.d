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
      '(lispy
        evil))

(defun lispy/init-lispy ()
  (use-package lispy
    :diminish lispy-mode " Ⓛ"
    :commands (lispy-mode lispy-enter-maybe lispy-toggle)
    :init
    (progn
      (defvar lispy-major-modes '(emacs-lisp-mode clojure-mode clojurescript-mode)
        "List of major modes associated with lispy.
Used by `lispy-enter-maybe'.")

      (defun lispy-enter-maybe ()
        "Enters lispy-mode if in a major mode listed in `lispy-major-modes'."
        (when (-contains? lispy-major-modes major-mode)
          (lispy-mode)))

      (defun lispy-exit ()
        "Exits lispy mode"
        (lispy-mode -1))

      (defun lispy-toggle ()
        "Toggles lispy mode."
        (interactive)
        (if lispy-mode (lispy-mode -1) (lispy-mode)))

      ;; require hydra before lispy loads
      (spacemacs/load-or-install-package 'hydra))
    :config
    (progn
      ;; Integrate lispy with evil jumper
      (with-eval-after-load "evil-jumper"
        (defadvice lispy-ace-symbol
            (before lispy-track-jump activate)
          (funcall #'evil-jumper--set-jump))
        (defadvice lispy-ace-paren
            (before lispy-track-jump activate)
          (funcall #'evil-jumper--set-jump)))

      ;; lispy settings
      (setq lispy-eval-display-style 'message
            lispy-no-permanent-semantic nil)

      ;; lispy keybindings
      (lispy-set-key-theme '(special c-digits lispy))
      (define-key lispy-mode-map (kbd "C-o") 'evil-jumper/backward)
      (define-key lispy-mode-map (kbd "TAB") 'evil-jumper/forward)
      (define-key lispy-mode-map (kbd "[") 'lispy-brackets)
      (define-key lispy-mode-map (kbd "{") 'lispy-braces)
      (define-key lispy-mode-map (kbd "<M-return>") nil)
      (define-key lispy-mode-map-lispy (kbd "<M-return>") nil)
      ;; use lispy-define-key for new specials
      ;; :inserter defaults to self-insert-command
      ;; but can be explicitly set as well
      ;; :override takes a quoted form and allows to add
      ;; conditionals, see abo-abo/lispy#111
      )))

(defun lispy/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config
    (progn
      (define-key evil-emacs-state-map (kbd "M-C-n") 'lispy-forward)
      (define-key evil-emacs-state-map (kbd "M-C-p") 'lispy-backward)
      (define-key evil-emacs-state-map (kbd "M-u") 'undo)
      (define-key evil-emacs-state-map (kbd "C-c l") 'lispy-toggle)
      (evil-leader/set-key "m m" 'lispy-mark-symbol)
      (add-hook 'evil-emacs-state-entry-hook #'lispy-enter-maybe)
      (add-hook 'evil-emacs-state-exit-hook #'lispy-exit))))
