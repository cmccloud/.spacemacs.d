;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
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
     material-theme
     color-theme-sanityinc-tomorrow
     base16-theme)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer.
   ;; Note that this setting can significantly increase loading time.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed.
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(zenburn
                         spacemacs-light
                         spacemacs-dark
                         solarized-light
                         solarized-dark
                         leuven
                         monokai)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("M+ 1mn"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; Default value is `cache'.
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f) is replaced.
   dotspacemacs-use-ido nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state nil
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 90
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one).
   dotspacemacs-highlight-delimiters nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   ;; Sequence of keys equivalent to <ESC>
   ;; default value: "fd"
   evil-escape-key-sequence "df")

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
      (evil-leader/set-key "ov" 'set-variable)

      ;; helm multi-files
      (define-key global-map (kbd "C-f") 'helm-multi-files)
      (define-key evil-normal-state-map (kbd "C-f") 'helm-multi-files)
      (define-key evil-evilified-state-map (kbd "C-f") 'helm-multi-files)
      (define-key evil-motion-state-map (kbd "C-f") 'helm-multi-files)
      (define-key evil-insert-state-map (kbd "C-f") 'helm-multi-files)))

  (spacemacs|use-package-add-hook helm
    :post-config
    (progn
      ;; sometimes we need to edit system files
      (defun helm-find-files-as-sudo ()
        (interactive)
        (helm-find-files-1 "/sudo::/"))

      ;; use mdfind for OSX
      (with-eval-after-load "helm-locate"
        (when (eql system-type 'darwin)
         (setq helm-locate-command "mdfind -name %s %s"
               helm-locate-fuzzy-match nil))) ; this is getting overwritten

      ;; add colors to remote connections
      (setq helm-ff-tramp-not-fancy nil)

      ;; helm for files settings
      (setq helm-for-files-preferred-list
            '(helm-source-buffers-list
              helm-source-files-in-current-dir
              helm-source-recentf
              helm-source-file-cache
              helm-source-locate
              helm-source-buffer-not-found))

      ;; helm projectile sources
      (with-eval-after-load "helm-projectile"
        (setq helm-for-files-preferred-list
              (-insert-at 1 'helm-source-projectile-files-list
                          helm-for-files-preferred-list)))

      ;; prefer helm when available
      (define-key global-map [remap info] 'helm-info-at-point)
      (define-key global-map [remap find-file] 'helm-find-files)
      (define-key global-map [remap list-buffers] 'helm-buffers-list)
      (define-key global-map [remap switch-to-buffer] 'helm-buffers-list)
      (define-key global-map [remap apropos-command] 'helm-apropos)
      (define-key global-map [remap info-emacs-manual] 'helm-info-emacs)

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
            erc-hl-nicks-minimum-contrast-ratio 3.5
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

(defun dotspacemacs/config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."

  ;; load custom file
  (setq custom-file "~/.spacemacs.d/custom.el")
  (load custom-file)

  ;; introductions are in order...
  (setq user-full-name "Christopher McCloud"
        user-login-name "cmccloud"
        user-mail-address "mccloud.christopher@gmail.com")

  ;; osx config
  ;; something like the old space cadet keyboard
  (when (eq system-type 'darwin)
    (setq mac-option-key-is-meta nil
          mac-command-key-is-meta t
          mac-command-modifier 'meta
          mac-option-modifier 'super)
    ;; have we installed coreutils?
    (if (executable-find "gls")
        (progn
          (setq insert-directory-program "gls")
          (setq dired-listing-switches "-al --group-directories-first"))
      (setq dired-listing-switches "-al")))

  ;; misc settings
  (setq powerline-default-separator nil
        avy-all-windows nil
        avy-background t
        doc-view-continuous t
        sp-show-pair-from-inside t      ; accounts for evil
        lispy-no-permanent-semantic t
        eshell-buffer-maximum-lines 2000
        paradox-github-token t
        magit-push-always-verify nil
        even-window-heights nil
        helm-locate-fuzzy-match nil
        flycheck-highlighting-mode nil
        echo-keystrokes .02
        smooth-scroll-margin 4               ; helps scroll lag for now
        cider-ovelays-use-font-lock t        ; misspelled
        markdown-open-command "marked")

  ;; fontify boolean operators
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("\\<\\(and\\|or\\|not\\)\\>" . 'font-lock-keyword-face)))

  ;; defaults
  (semantic-mode)
  (global-semanticdb-minor-mode)
  (spacemacs/toggle-vi-tilde-fringe-off)
  (fringe-mode 4)
  (cl-lib-highlight-initialize)
  (add-hook 'emacs-lisp-mode-hook
            'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook
            'rainbow-delimiters-mode)
  (with-eval-after-load "flycheck"
    (flycheck-clojure-setup))

  ;; build file cache
  (file-cache-add-directory-list
   '("~/Documents/Programming Books/"))

  ;; performance
  (setq-default bidi-display-reordering nil
                max-lisp-eval-depth 30000
                max-specpdl-size 30000
                large-file-warning-threshold 25000000))
