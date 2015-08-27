;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.25)
 '(ahs-idle-timer 0 t)
 '(ahs-inhibit-face-list nil)
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(erc-autoaway-idle-seconds 600)
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-current-nick-highlight-type (quote all))
 '(erc-fill-mode t)
 '(erc-hl-nicks-mode t)
 '(erc-ignore-list nil)
 '(erc-irccontrols-mode t)
 '(erc-join-buffer (quote bury))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-list-mode t)
 '(erc-log-channels-directory "/Users/Macnube/.emacs.d/.cache/erc-logs")
 '(erc-log-insert-log-on-open nil)
 '(erc-log-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-server-coding-system (quote (utf-8 . utf-8)))
 '(erc-services-mode t)
 '(erc-social-graph-dynamic-graph t)
 '(erc-stamp-mode t)
 '(erc-track-exclude-server-buffer t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(erc-track-position-in-mode-line t)
 '(erc-track-shorten-aggressively (quote max))
 '(erc-youtube-mode t)
 '(helm-M-x-fuzzy-match t)
 '(helm-ag-always-set-extra-option t)
 '(helm-always-two-windows t)
 '(helm-apropos-fuzzy-match t)
 '(helm-bookmark-show-location t)
 '(helm-buffers-fuzzy-matching t)
 '(helm-descbinds-mode t)
 '(helm-descbinds-window-style (quote split))
 '(helm-display-function (quote spacemacs//display-helm-at-bottom))
 '(helm-display-header-line nil)
 '(helm-echo-input-in-header-line t)
 '(helm-file-cache-fuzzy-match t)
 '(helm-imenu-fuzzy-match t)
 '(helm-lisp-fuzzy-completion t)
 '(helm-locate-fuzzy-match t)
 '(helm-mode t)
 '(helm-recentf-fuzzy-match t)
 '(helm-semantic-fuzzy-match t)
 '(helm-split-window-in-side-p t)
 '(helm-swoop-speed-or-color t)
 '(helm-swoop-split-with-multiple-windows t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(lispy-completion-method (quote default))
 '(lispy-eval-display-style (quote overlay))
 '(lispy-no-permanent-semantic t)
 '(lispy-occur-backend (quote ivy))
 '(lispy-visit-method (quote projectile))
 '(magit-diff-use-overlays nil)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(rainbow-delimiters-max-face-count 1)
 '(ring-bell-function (quote ignore) t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
