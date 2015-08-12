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
     org
     git
     version-control
     emacs-lisp
     clojure
     javascript
     html
     markdown
     ;; my configuration layers
     lispy
     persp-mode
     popwin-pop-repl
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages
   '(
     material-theme
     color-theme-sanityinc-tomorrow
     base16-theme
     )
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
                               :size 15
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
      (defun helm-find-contrib-file ()
        "Runs helm find files on spacemacs contrib folder"
        (interactive)
        (helm-find-files-1 (expand-file-name (concat user-emacs-directory "contrib/"))))

      (defun helm-find-spacemacs-file ()
        "Runs helm find files on spacemacs directory"
        (interactive)
        (helm-find-files-1 (expand-file-name (concat user-emacs-directory "spacemacs/"))))

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
      (define-key global-map [remap find-spacemacs-file] 'helm-find-spacemacs-file)
      (define-key global-map [remap find-contrib-file] 'helm-find-contrib-file)
      (define-key global-map [remap isearch-forward] 'helm-swoop)
      (define-key global-map [remap info-emacs-manual] 'helm-info-emacs)
      (define-key global-map [remap persp-switch] 'helm-perspectives)

      ;; user reserved key-bindings
      (define-key global-map (kbd "C-c r") 'helm-semantic-or-imenu)
      (define-key global-map (kbd "C-c e") 'eval-defun)))

  (spacemacs|use-package-add-hook popwin
    :post-config
    (progn
      ;; additional popwin managed windows
      (defun spacemacs/popwin-manage-window (&rest windows)
        "Adds window to `popwin:special-display-config' with default settings."
        (let ((settings '(:dedicated t :position bottom :stick t :noselect nil :height 0.4)))
          (while windows
            (push (cons (pop windows) settings) popwin:special-display-config))))

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

  ;; introductions!
  (setq user-full-name "Christopher McCloud"
        user-mail-address "mccloud.christopher@gmail.com")

  ;; command as meta, option as super
  (setq mac-option-key-is-meta nil
        mac-command-key-is-meta t
        mac-command-modifier 'meta
        mac-option-modifier 'super)

  ;; misc settings
  (setq powerline-default-separator nil
        avy-all-windows nil
        avy-background t
        doc-view-continuous t
        lispy-no-permanent-semantic t
        paradox-github-token t
        even-window-heights nil
        smooth-scroll-margin 4          ; helps scroll lag for now
        cider-ovelays-use-font-lock t   ; misspelled
        markdown-open-command "marked")

  ;; default toggles
  (semantic-mode)
  (spacemacs/toggle-vi-tilde-fringe-off)

  ;; build file cache
  (file-cache-add-directory-list
   '("~/Documents/Programming Books/"))

  ;; configure mu4e
  ;; offlineimap file :: ~/.offlineimaprc
  (use-package mu4e
    :load-path "/usr/local/share/emacs/site-lisp/mu4e"
    :commands (mu4e mu4e-compose-new)
    :init
    (progn
      (evil-leader/set-key "am" 'mu4e)
      (global-set-key (kbd "C-c m") 'mu4e-compose-new))
    :config
    (progn
      ;; TODO: keybinding support
      ;; TODO: folder nametransforms
      ;; TODO: gmail tag importing
      ;; TODO: custom bookmarks
      ;;   - manage mailing lists
      ;; TODO: contacts - completion
      ;; TODO: spell check
      ;; TODO: custom homescreen
      ;; TODO: org-mode integration

      (require 'mu4e-contrib)
      ;; keybindings
      (evilify mu4e-main-mode mu4e-main-mode-map
               "j" 'mu4e~headers-jump-to-maildir)
      (evilify mu4e-headers-mode mu4e-headers-mode-map)
      (evilify mu4e-view-mode mu4e-view-mode-map
               "J" 'mu4e-view-headers-next
               "K" 'mu4e-view-headers-prev)

      ;; mailbox shortcuts
      (setq mu4e-maildir-shortcuts
            '(("/INBOX" . ?i)))

      ;; bookmarks
      (setq mu4e-bookmarks
            '(("flag:flagged AND NOT flag:trashed"
               "Flagged Messages" ?f)
              ("flag:unread AND date:7d..now AND NOT flag:trashed"
               "Latest Unread Messages" ?u)
              ("date:7d..now AND NOT flag:trashed"
               "Latest Messages" ?l)
              ("flag:unread AND NOT flag:trashed"
               "All Unread Messages" ?a)
              ("date:today..now AND NOT flag:trashed"
               "Today's messages" ?t)))

      ;; include signature
      (setq mu4e-compose-signature
            (concat "Christopher McCloud\n"
                    "mccloud.christopher@gmail.com\n")
            mu4e-compose-signature-auto-include t)

      (setq mu4e-get-mail-command "offlineimap -q"
            mu4e-attachment-dir "~/Downloads"
            mu4e-update-interval 300
            mu4e-confirm-quit nil
            mu4e-completing-read-function 'helm--completing-read-default
            mu4e-view-show-images t
            mu4e-view-prefer-html t
            mu4e-compose-dont-reply-to-self t
            mu4e-hide-index-messages t
            mu4e-compose-complete-only-personal nil
            mu4e-html2text-command 'mu4e-shr2text
            mu4e-headers-skip-duplicates t
            mu4e-view-show-addresses t
            mu4e-sent-messages-behavior 'delete
            message-kill-buffer-on-exit t)

      ;; maildirs
      (setq mu4e-maildir "~/.mail/gmail"
            mu4e-drafts-folder "/[Gmail].Drafts"
            mu4e-sent-folder "/[Gmail].Sent Mail"
            mu4e-trash-folder "/[Gmail].Trash")

      ;; sending mail
      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-stream-type 'starttls
            smtpmail-default-smtp-server "smtp.gmail.com"
            smtpmail-smtp-server "smtp.gmail.com"
            smtpmail-smtp-service 587)))

  ;; performance
  (setq bidi-display-reordering nil)
  (setq max-lisp-eval-depth 30000)
  (setq max-specpdl-size 30000)
  (setq large-file-warning-threshold 25000000))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(setq custom-file "~/.spacemacs.d/custom.el")
(load custom-file)
