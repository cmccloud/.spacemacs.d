;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs-base)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-return-key-behavior nil
                      auto-completion-enable-snippets-in-popup t)
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'eshell)
     evil-snipe
     syntax-checking
     spell-checking
     erc
     org
     git
     github
     version-control
     emacs-lisp
     clojure
     javascript
     html
     markdown
     lua
     ;; my configuration layers
     lispy
     lively
     doc-view-compatibility
     evil-cleverparens
     edebug
     persp-mode
     popwin-pop-repl
     mu4e)
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
   '(cl-lib-highlight
     flycheck-clojure
     helm-gtags
     monokai-theme
     arjen-grey-theme
     zenburn-theme
     leuven-theme
     material-theme
     color-theme-sanityinc-tomorrow
     base16-theme)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         solarized-light
                         solarized-dark
                         spacemacs-light
                         spacemacs-dark)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Input"
                               :size 14
                               :weight normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters nil
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Sequence of keys equivalent to <ESC>
   ;; default value: "fd"
   evil-escape-key-sequence "df"))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."
  ;; load user auth file
  (defvar user-auth-file "~/.spacemacs.d/auth.el"
    "Location of gitignored user-authentication file")
  (when (file-exists-p user-auth-file)
    (load user-auth-file))

  ;; ---------------------------------
  ;; Use Package Hooks
  ;; ---------------------------------
  (spacemacs|use-package-add-hook evil
    :post-config
    (progn
      ;; restore universal argument
      (define-key global-map (kbd "C-u") 'universal-argument)
      (define-key evil-normal-state-map (kbd "C-u") 'universal-argument)
      (define-key evil-motion-state-map (kbd "C-u") 'universal-argument)
      (define-key evil-insert-state-map (kbd "C-u") 'universal-argument)
      (define-key evil-evilified-state-map (kbd "C-u") 'universal-argument)
      (define-key evil-emacs-state-map (kbd "C-u") 'universal-argument)

      ;; buffer switching
      (define-key evil-normal-state-map "J" 'spacemacs/next-useful-buffer)
      (define-key evil-normal-state-map "K" 'spacemacs/previous-useful-buffer)

      ;; iedit-mode
      (define-key global-map (kbd "C-;") 'iedit-mode)
      (define-key evil-normal-state-map (kbd "C-;") 'iedit-mode)
      (define-key evil-insert-state-map (kbd "C-;") 'iedit-mode)

      ;; user reserved key-bindings
      (evil-leader/set-key "od" 'dired-jump)
      (evil-leader/set-key "or" 'popwin-pop-repl)
      (evil-leader/set-key "ov" 'set-variable)))

  (spacemacs|use-package-add-hook evil-escape
    :post-config
    (progn
      ;; supress evil escape when lispy mode active
      (push (lambda () (not (bound-and-true-p lispy-mode)))
            evil-escape-suppressed-predicates)))

  (spacemacs|use-package-add-hook evil-snipe
    :post-init
    (progn
      (setq evil-snipe-enable-alternate-f-and-t-behaviors t)))

  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      ;; helm for files settings
      (setq helm-for-files-preferred-list
            '(helm-source-buffers-list
              helm-source-files-in-current-dir
              helm-source-recentf
              helm-source-file-cache
              helm-source-locate
              helm-source-buffer-not-found)
            ;; add colors to remote connections
            helm-ff-tramp-not-fancy nil)

      ;; helm projectile sources
      (with-eval-after-load "helm-projectile"
        (setq helm-for-files-preferred-list
              (-insert-at 1 'helm-source-projectile-files-list
                          helm-for-files-preferred-list)))

      ;; prefer helm when available
      (define-key global-map [remap info] 'helm-info-at-point)
      (define-key global-map [remap ido-find-file] 'helm-find-files)
      (define-key global-map [remap find-file] 'helm-find-files)
      (define-key global-map [remap list-buffers] 'helm-buffers-list)
      (define-key global-map [remap switch-to-buffer] 'helm-buffers-list)
      (define-key global-map [remap apropos-command] 'helm-apropos)
      (define-key global-map [remap info-emacs-manual] 'helm-info-emacs)
      (define-key global-map (kbd "C-x C-f") 'helm-find-files)
      (define-key global-map (kbd "C-x C-b") 'helm-multi-files)
      (define-key global-map (kbd "C-x b") 'helm-multi-files)

      ;; user keybinds
      (define-key global-map (kbd "C-c t") 'helm-gtags-select)
      (define-key global-map (kbd "C-s") 'helm-swoop)
      (define-key global-map (kbd "C-c r") 'helm-semantic-or-imenu)
      (define-key global-map (kbd "C-c e") 'eval-defun)))

  (spacemacs|use-package-add-hook erc
    :post-config
    (progn
      (defun erc-slack-connect ()
        "Quick connect to hrx slack."
        (interactive)
        (erc-ssl :server "hackreactorx.irc.slack.com"
                 :port 6697
                 :nick "mccloud.christopher"
                 :password user-slack-irc-password))

      (defun erc-gitter-connect ()
        "Quick connect to irc.gitter.im"
        (interactive)
        ;; clean up old buffers if they exist
        (dolist (buf '("irc.gitter.im:6667" "#syl20bnr/spacemacs"))
          (when (get-buffer buf) (kill-buffer buf)))
        (erc-ssl :server "irc.gitter.im"
                 :port 6667
                 :nick "cmccloud"
                 :password user-gitter-irc-password))

      (defun erc-freenode-connect ()
        "Quick connect to irc.freenode.net"
        (interactive)
        ;; clean up old buffers if they exist
        (dolist (buf '("irc.freenode.net:6667" "#emacs" "#clojure"))
          (when (get-buffer buf) (kill-buffer buf)))
        (erc :server "irc.freenode.net"
             :port 6667
             :nick "cmccloud"))

      (evil-leader/set-key
        "aig" 'erc-gitter-connect
        "aif" 'erc-freenode-connect
        "ais" 'erc-slack-connect)

      ;; if imagemagick isn't supported, we don't want inline images
      (unless (fboundp 'imagemagick-types)
        (setq erc-modules (-remove-item 'image erc-modules)))

      (setq erc-autojoin-channels-alist
            '(("1\\.0\\.0" "#syl20bnr/spacemacs")
              ("freenode.net" "#emacs" "#clojure"))
            erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "MODE" "353")
            erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE" "353")
            erc-track-exclude-server-buffer t
            erc-track-position-in-mode-line t
            erc-join-buffer 'bury
            erc-hl-nicks-minimum-contrast-ratio 2.5
            erc-hl-nicks-color-contrast-strategy '(invert contrast)
            erc-current-nick-highlight-type 'all
            erc-log-insert-log-on-open nil
            erc-track-shorten-aggressively 'max
            erc-prompt-for-nickserv-password nil)

      ;; we dont need paren highlighting
      (add-hook 'erc-mode-hook 'turn-off-show-smartparens-mode)))

  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn
      ;; additional popwin managed windows
      (defun spacemacs/popwin-manage-window (&rest windows)
        "Adds window to `popwin:special-display-config' with default settings."
        (while windows
          (push (cons (pop windows)
                      '(:dedicated t :position bottom :stick t
                                   :noselect nil :height 0.4))
                popwin:special-display-config)))

      (spacemacs/popwin-manage-window
       "*Compile-Log*"
       "*Process List*"
       "*Agenda Commands*"
       "*trace-output*"
       "*Diff*"))))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; load custom file
  (setq custom-file "~/.spacemacs.d/custom.el")
  (load custom-file)

  ;; load keybindings (wip)
  (when (file-exists-p "~/.spacemacs.d/keybinds.el")
    (load "~/.spacemacs.d/keybinds.el"))

  ;; introductions are in order...
  (setq user-full-name "Christopher McCloud"
        user-login-name "cmccloud"
        user-mail-address "mccloud.christopher@gmail.com")

  ;; osx config
  (when (eq system-type 'darwin)
    ;; something like the old space cadet keyboard
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'super)

    ;; have we installed coreutils?
    (if (executable-find "gls")
        (setq insert-directory-program "gls"
              dired-listing-switches "-al --group-directories-first")
      (setq dired-listing-switches "-al"))

    ;; use mdfind rather than locate
    (with-eval-after-load "helm-locate"
      (setq helm-locate-command "mdfind -name %s %s"
            helm-locate-fuzzy-match nil))

    ;; better grep
    (when (executable-find "ggrep")
      (with-eval-after-load "helm"
        (setq helm-grep-default-command
              "ggrep --color=always -a -d skip %e -n%cH -e %p %f"
              helm-grep-default-recurse-command
              "ggrep --color=always -a -d recurse %e -n%cH -e %p %f")))

    ;; used marked2.app for markdown live preview
    (when (executable-find "marked")
      (setq markdown-open-command "marked")))

  ;; misc settings
  (setq powerline-default-separator nil
        avy-all-windows nil
        avy-background t
        evil-cross-lines t
        doc-view-continuous t
        sp-show-pair-from-inside t
        eshell-buffer-maximum-lines 2000
        cursor-in-non-selected-windows nil
        paradox-github-token t
        magit-push-always-verify nil
        even-window-heights nil
        flycheck-highlighting-mode nil
        echo-keystrokes .02
        smooth-scroll-margin 4          ; helps scroll lag for now
        cider-ovelays-use-font-lock t)

  ;; keybinds
  (evil-leader/set-key "m M-RET" 'avy-goto-word-or-subword-1)

  ;; defaults
  (semantic-mode)
  (spacemacs/toggle-vi-tilde-fringe-off)
  (fringe-mode 4)
  (cl-lib-highlight-initialize)
  (add-hook 'emacs-lisp-mode-hook
            'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook
            'rainbow-delimiters-mode)
  (with-eval-after-load "flycheck"
    (flycheck-clojure-setup))
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)

  ;; performance
  (setq-default bidi-display-reordering nil
                jit-lock-stealth-time 3
                max-lisp-eval-depth 30000
                cursor-in-non-selected-windows nil
                helm-swoop-speed-or-color nil
                max-specpdl-size 30000
                large-file-warning-threshold 25000000))
