;; -*- mode: emacs-lisp -*-
;; User defined keybindings

;; restore universal argument
(define-key global-map (kbd "C-u") 'universal-argument)
(define-key evil-normal-state-map (kbd "C-u") 'universal-argument)
(define-key evil-motion-state-map (kbd "C-u") 'universal-argument)
(define-key evil-insert-state-map (kbd "C-u") 'universal-argument)
(define-key evil-evilified-state-map (kbd "C-u") 'universal-argument)
(define-key evil-emacs-state-map (kbd "C-u") 'universal-argument)

;; prefer helm when available
(define-key global-map [remap info] 'helm-info-at-point)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap switch-to-buffer] 'helm-buffers-list)
(define-key global-map [remap apropos-command] 'helm-apropos)
(define-key global-map [remap find-spacemacs-file] 'helm-find-spacemacs-file)
(define-key global-map [remap find-contrib-file] 'helm-find-contrib-file)
(define-key global-map [remap isearch-forward] 'helm-swoop)
(define-key global-map [remap helm-pp-bookmarks] 'helm-filtered-bookmarks) ; fixed in #2247
(define-key global-map [remap info-emacs-manual] 'helm-info-emacs)
(define-key global-map [remap persp-switch] 'helm-perspectives)

;; lispy bindings
(define-key lispy-mode-map (kbd "C-?") 'helm-descbinds)
(define-key lispy-mode-map (kbd "C-f") 'lispy-forward)
(define-key lispy-mode-map (kbd "C-d") 'lispy-backward)
(define-key lispy-mode-map (kbd "M-u") 'lispy-undo)
(define-key lispy-mode-map (kbd "[") 'lispy-brackets)
(define-key lispy-mode-map (kbd "{") 'lispy-braces)

;; helm multi-files
(define-key global-map (kbd "C-f") 'helm-multi-files)
(define-key evil-normal-state-map (kbd "C-f") 'helm-multi-files)
(define-key evil-evilified-state-map (kbd "C-f") 'helm-multi-files)
(define-key evil-motion-state-map (kbd "C-f") 'helm-multi-files)
(define-key evil-insert-state-map (kbd "C-f") 'helm-multi-files)

;; helm projectile
(define-key global-map (kbd "C-p") 'helm-projectile)
(define-key evil-normal-state-map (kbd "C-p") 'helm-projectile)
(define-key evil-evilified-state-map (kbd "C-p") 'helm-projectile)
(define-key evil-motion-state-map (kbd "C-p") 'helm-projectile)
(define-key evil-insert-state-map (kbd "C-p") 'helm-projectile)

;; iedit-mode
(define-key global-map (kbd "C-;") 'iedit-mode)
(define-key evil-normal-state-map (kbd "C-;") 'iedit-mode)
(define-key evil-insert-state-map (kbd "C-;") 'iedit-mode)

;; super bindings
(global-set-key (kbd "s-=") 'zoom-frm-in)
(global-set-key (kbd "s--") 'zoom-frm-out)
(global-set-key (kbd "s-k") nil)

;; buffer switching
(define-key evil-normal-state-map "J" 'spacemacs/next-useful-buffer)
(define-key evil-normal-state-map "K" 'spacemacs/previous-useful-buffer)

;; user reserved key-bindings
(evil-leader/set-key "or" 'mcc-pop-repl)
(evil-leader/set-key "ov" 'set-variable)
(define-key global-map (kbd "C-c r") 'helm-semantic-or-imenu)
(define-key global-map (kbd "C-c e") 'eval-defun)
(define-key global-map (kbd "C-c i") 'mcc-instrument-with-edebug)
(define-key global-map (kbd "C-c l") 'lispy-toggle)

