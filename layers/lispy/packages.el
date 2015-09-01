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
    :preface
    (progn
      (defvar lispy-major-modes '(emacs-lisp-mode clojure-mode)
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
        (if lispy-mode (lispy-mode -1) (lispy-mode))))
    :config
    (progn
      ;; lispy keybindings
      (lispy-set-key-theme '(special paraedit c-digits))
      (define-key lispy-mode-map (kbd "C-c l") 'lispy-toggle)
      (define-key lispy-mode-map (kbd "C-f") 'helm-multi-files)
      (define-key lispy-mode-map (kbd "C-?") 'helm-descbinds)
      (define-key lispy-mode-map (kbd "C-o") 'evil-jumper/backward)
      (define-key lispy-mode-map (kbd "TAB") 'evil-jumper/forward)
      (define-key lispy-mode-map (kbd "C-n") 'lispy-forward)
      (define-key lispy-mode-map (kbd "C-p") 'lispy-backward)
      (define-key lispy-mode-map (kbd "C-l") 'forward-char)
      (define-key lispy-mode-map (kbd "C-h") 'backward-char)
      (define-key lispy-mode-map (kbd "M-u") 'lispy-undo)
      (define-key lispy-mode-map (kbd "[") 'lispy-brackets)
      (define-key lispy-mode-map (kbd "{") 'lispy-braces)
      (define-key lispy-mode-map (kbd "M-p") 'lispy-move-up)
      (define-key lispy-mode-map (kbd "M-n") 'lispy-move-down)
      (define-key lispy-mode-map (kbd "C-s") 'helm-occur)
      (define-key lispy-mode-map (kbd "DEL") 'lispy-delete-backward)

      ;; use lispy-define-key for new specials
      ;; :inserter defaults to self-insert-command
      ;; but can be explicitly set as well
      ;; :override takes a quoted form and allows to add
      ;; conditionals, see abo-abo/lispy#111
      (lispy-define-key lispy-mode-map (kbd "b") 'lispy-barf)
      (lispy-define-key lispy-mode-map (kbd "s") 'lispy-slurp)
      (lispy-define-key lispy-mode-map (kbd "y") 'lispy-new-copy)
      (lispy-define-key lispy-mode-map (kbd "p") 'lispy-yank)
      (lispy-define-key lispy-mode-map (kbd "g") 'lispy-goto-local)
      (lispy-define-key lispy-mode-map (kbd "G") 'lispy-goto))))

(defun lispy/pre-init-evil ()
  (spacemacs|use-package-add-hook evil
    :post-config
    (progn
      (define-key evil-emacs-state-map (kbd "C-c l") 'lispy-toggle)
      (add-hook 'evil-emacs-state-entry-hook #'lispy-enter-maybe)
      (add-hook 'evil-emacs-state-exit-hook #'lispy-exit))))
