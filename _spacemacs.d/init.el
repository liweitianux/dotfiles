;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=lisp:
;;
;; Spacemacs: A community-driven Emacs distribution
;; The best editor is neither Emacs nor Vim, it's Emacs *and* Vim!
;; http://spacemacs.org/
;; https://github.com/syl20bnr/spacemacs
;;
;; ~/.spacemacs
;; ~/.spacemacs.d/init.el
;;
;; Aaron LI
;; Created: 2016-04-30
;; Updated: 2016-06-18
;;


(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
  You should not put any user code in this function besides modifying
  the variable values."
  (setq-default
   ;; Base distribution to use.
   ;; This is a layer contained in the directory `+distribution'.
   ;; For now available distributions are `spacemacs-base' or `spacemacs'.
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/').
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/layers/")
   ;; List of configuration layers to load.
   ;; If it is the symbol `all' instead of a list then all discovered
   ;; layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-return-key-behavior 'complete
                      auto-completion-complete-with-key-sequence "kj"
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-snippets-in-popup t
                      ;auto-completion-enable-help-tooltip t
                      ;; Provided by `company-statistics'
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory
                      "~/.spacemacs.d/snippets/")
     ;; better-defaults
     c-c++
     (chinese :variables chinese-enable-fcitx t)
     clojure
     emacs-lisp
     games
     git
     latex
     markdown
     (mu4e :variables
           mu4e-installation-path "~/.spacemacs.d/local/mu4e")
     octave
     org
     python
     ruby
     scheme
     (shell :variables
            shell-default-shell 'eshell
            shell-default-term-shell "zsh"
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     spell-checking
     syntax-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer.  If you need some configuration for these
   ;; packages, then consider creating a layer.  You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '()
   ;; List of packages/extensions that will not be installed or loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil, spacemacs will delete any orphan packages, i.e. packages
   ;; that are declared in a layer which is not a member of the list
   ;; `dotspacemacs-configuration-layers'. (default t)
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
   ;; If non-nil, ELPA repositories are contacted via HTTPS whenever it's
   ;; possible.  Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update nil
   ;; One of `vim', `emacs' or `hybrid'.  Evil is always enabled but if
   ;; the variable is `emacs' then the `holy-mode' is enabled at startup.
   ;; `hybrid' uses emacs key bindings for vim's insert mode, but otherwise
   ;; leaves evil unchanged. (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner.  Default value is `official', it displays
   ;; the official spacemacs logo.  An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory.  A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'random
   ;; List of items to show in the startup buffer.  If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; Number of recent files to show in the startup buffer.  Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   dotspacemacs-startup-recent-list-size 5
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font.  `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`.  Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like
   ;; in Vim with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil then `ido' replaces `helm' for some commands.  For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
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
   ;; Which-key frame position.  Possible values are `right', `bottom' and
   ;; `right-then-bottom'.  right-then-bottom tries to display the frame to
   ;; the right; if there is insufficient space it displays it at the
   ;; bottom. (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading.
   ;; This may increase the boot time on some systems and emacs builds,
   ;; set it to nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
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
   ;; If non nil smooth scrolling (native-scrolling) is enabled.  Smooth
   ;; scrolling overrides the default behavior of Emacs which re-centers the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives.  If set to `relative', also turns on relative
   ;; line numbers. (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters.  Possible values are `any',
   ;; `current', `all' or `nil'.  Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names.  Spacemacs uses the first
   ;; installed tool of the list.  Supported tools are `ag', `pt', `ack'
   ;; and `grep'. (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer.  Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
  It is called immediately after `dotspacemacs/init'.
  You are free to put almost any user code here.  The exception is
  org related code, which should be placed in `dotspacemacs/user-config'."
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
  This function is called at the very end of Spacemacs initialization after
  layers configuration. You are free to put any user code."
  ;;
  (setq-default tab-width 4)
  (setq-default fill-column 78)
  ;; Put a newline at the end of file if there isn't already one there
  (setq-default require-final-newline t)
  ;; Separate line number from text using a vertical line
  (setq linum-format "%4d\u2502")
  ;;
  ;; Set the default size for all frames
  (add-to-list 'default-frame-alist '(width . 86))
  (add-to-list 'default-frame-alist '(height . 34))
  ;;
  ;; Prevent the visual selection overriding the system clipboard
  (fset 'evil-visual-update-x-selection 'ignore)
  ;; Prevent transferring data to clipboard manager when exiting
  (setq x-select-enable-clipboard-manager nil)
  ;; Aggressively prevent `kill' and `yank' from accessing the clipboard
  (setq x-select-enable-clipboard nil)
  ;;
  ;; Make whitespace visible by enabling the `whitespace-mode'
  ;; Credit: http://ergoemacs.org/emacs/whitespace-mode.html
  ;; Make `whitespace-mode' use just basic colors
  (setq whitespace-style
        '(face tabs trailing lines-tail indentation
               space-before-tab space-after-tab newline
               tab-mark newline-mark))
  ;; Change the symbols used for different whitespace characters
  (setq whitespace-display-mappings
        ;; All numbers are Unicode codepoint in decimal.
        ;; Try `(insert-char <number>)' to see it.
        ;; 9: tab; 10: line feed; 32: space; 46: full stop 「.」
        ;; 183: middle dot 「·」; 9655: white right-pointing triangle 「▷」
        '((space-mark 32 [183] [46])
          (newline-mark 10 [182 10])
          (tab-mark 9 [9655 9] [92 9])
          ))
  ;; Highlight the line part that goes beyond `whitespace-line-column'
  (setq whitespace-line-column fill-column)
  (global-whitespace-mode)
  ;;
  ;; Enable `company-mode' globally
  (global-company-mode)
  (setq company-idle-delay 0.1)
  ;;
  ;; Wrap long lines at the space or tab character
  (setq-default word-wrap t)
  ;; Wrap long lines & visual line navigation
  ;; Credit: https://emacs.stackexchange.com/a/19364
  (spacemacs/toggle-truncate-lines-off)
  (spacemacs/toggle-visual-line-navigation-on)
  ;;
  ;; Change `powerline' separator
  (setq powerline-default-separator nil)
  ;; Set monospaced font size for Chinese
  (spacemacs//set-monospaced-font "Source Code Pro" "WenQuanYi Zen Hei" 14 16)
  ;; Display tildes in the fringe on empty lines
  (global-vi-tilde-fringe-mode)
  ;;
  ;; Enable fill column indicator in most modes, except for `org-mode'
  ;; Credit: https://github.com/syl20bnr/spacemacs/issues/4506
  ;; WARNING:
  ;;   `fci-mode' has issue with `truncate-lines'
  ;;   https://github.com/alpaker/Fill-Column-Indicator/issues/26
  ;(setq fci-handle-truncate-lines nil)
  ;(add-hook 'prog-mode-hook 'turn-on-fci-mode)
  ;(add-hook 'text-mode-hook 'turn-on-fci-mode)
  ;(add-hook 'org-mode-hook  'turn-off-fci-mode 'append)
  ;;
  ;; mu4e: https://github.com/djcb/mu
  (push "~/.spacemacs.d/config" load-path)
  (require 'aly-mu4e-config nil t)
  ;;
  ;; Set custom file location instead of using this file
  (setq custom-file "~/.spacemacs/custom.el")
  (if (file-readable-p custom-file)
      (load custom-file))
  )


;;; init.el ends here
