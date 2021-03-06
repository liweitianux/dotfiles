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
;; The template is located at `core/templates/.spacemacs.template`
;; Sync date: 2017-05-16
;;
;; Aaron LI
;; Created: 2016-04-30
;;


;; DEBUG: time the time required to load a package
;; Credit:
;; https://github.com/syl20bnr/spacemacs/issues/2705#issuecomment-133006706
;(defadvice require (around my-require activate)
;  (let ((start (current-time))
;        res delta)
;    (setq res ad-do-it)
;    (setq delta (float-time (time-since start)))
;    (when (> delta 0.001)
;      (message "Required %s: %s sec" (ad-get-arg 0) delta))
;    res))


(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
  You should not put any user code in this function besides modifying
  the variable values."
  (setq-default
   ;; Base distribution to use.
   ;; This is a layer contained in the directory `+distribution'.
   ;; For now available distributions are `spacemacs-base' or `spacemacs'.
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a
   ;; file with a supported type is opened).
   ;; Possible values are `all', `unused' and `nil'.
   ;; `unused' will lazy install only unused layers (i.e. layers not listed
   ;; in variable `dotspacemacs-configuration-layers'),
   ;; `all' will lazy install any layer that support lazy installation even
   ;; the layers listed in `dotspacemacs-configuration-layers'.
   ;; `nil' disable the lazy installation feature and you have to explicitly
   ;; list a layer in the variable `dotspacemacs-configuration-layers' to
   ;; install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (e.g., `~/.mycontribs/').
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
                      auto-completion-enable-sort-by-usage t
                      auto-completion-private-snippets-directory
                        "~/.spacemacs.d/snippets/")
     bibtex
     c-c++
     (chinese :variables chinese-enable-fcitx t)
     (clojure :variables clojure-enable-fancify-symbols t)
     colors
     emacs-lisp
     ess  ; For `R' programming language
     games
     git
     html
     javascript
     latex
     markdown
     (mu4e :variables
           mu4e-enable-mode-line t
           mu4e-installation-path "~/local/mu/share/emacs/site-lisp/mu4e")
     octave
     org
     python
     (ranger :variables
             ranger-show-preview t
             ;; Do not show hidden files by default
             ranger-show-hidden nil
             ;; Cleanup opened buffers when disabling the minor mode
             ranger-cleanup-on-disable t
             ;; Delay time to update the footer information
             ranger-footer-delay 0.1
             ;; Delay time to preview the file
             ranger-preview-delay 0.2
             ;; File size in MB to prevent preview of files
             ranger-max-preview-size 1)
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
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     xkcd
     yaml
     ;; Custom layers
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer.  If you need some configuration for these
   ;; packages, then consider creating a layer.  You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; XXX: spacemacs regard `evil-leader' as orphan package, unknown!
     evil-leader
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; List of packages that will not be installed or loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall
   ;; any unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't
   ;; uninstall them if they become unused.
   ;; `all' installs *all* packages supported by Spacemacs and never
   ;; uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only
   ))


(defun dotspacemacs/init ()
  "Initialization function.
  This function is called at the very startup of Spacemacs initialization
  before layers configuration.
  You should not put any user code in there besides modifying the variable
  values."
  ;; This `setq-default' sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil, ELPA repositories are contacted via HTTPS whenever it's
   ;; possible.  Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'.
   ;; Note that checking for new versions works via git commands, thus it
   ;; calls GitHub services whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory.
   ;; For example, to use different package directories for different
   ;; Emacs versions, set this to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
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
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`.  If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-dark
                         solarized-light
                         monokai
                         leuven)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts.
   ;; `powerline-scale' allows to quickly tweak the mode-line size to
   ;; make separators look not too crappy.
   dotspacemacs-default-font '("M+ 1mn"
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
   ;; The key used for Emacs commands (M-x) (after pressing on the leader
   ;; key). (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; These variables control whether separate commands are bound in the
   ;; GUI to the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under
   ;; <C-i> and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so
   ;; this only works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if
   ;; used there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil
   ;; ex-command. (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically
   ;; upon start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues.  Opening a file literally means
   ;; that no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm.  If set to `always', force fuzzy
   ;; matching in all non-asynchronous sources.  If set to `source',
   ;; preserve individual source settings.  Else, disable fuzzy matching
   ;; in all sources. (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position.
   ;; Possible values are `right', `bottom' and `right-then-bottom'.
   ;; `right-then-bottom' tries to display the frame to the right;
   ;; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
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
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys.
   ;; (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled.  Smooth
   ;; scrolling overrides the default behavior of Emacs which re-centers the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all
   ;; `prog-mode' and `text-mode' derivatives.  If set to `relative',
   ;; line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers nil
   ;; Code folding method.  Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil pressing the closing parenthesis `)' key in insert mode
   ;; passes over any automatically added closing parenthesis, bracket,
   ;; quote, etc…  This can be temporary disabled by pressing `C-q'
   ;; before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters.  Possible values are `any',
   ;; `current', `all' or `nil'.  Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t
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
   ;; NOTE: Currently, cannot *locally* disable whitespace cleanup when it
   ;;       is globally enabled with modes `all' or `trailing', but `changed'
   ;;       mode works OK.
   ;;       See: https://github.com/syl20bnr/spacemacs/issues/8221
   ;dotspacemacs-whitespace-cleanup 'trailing
   dotspacemacs-whitespace-cleanup 'changed
   ))


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
  It is called immediately after `dotspacemacs/init'.
  You are free to put almost any user code here.  The exception is
  org related code, which should be placed in `dotspacemacs/user-config'."

  ;; Helm uses Tramp which tries to figure out some SSH/DNS settings at
  ;; startup.  However, the ISP may redirect non-existing addresses to
  ;; their own servers, which causes long timeouts and blocks the startup.
  ;; Credit:
  ;; + Spacemacs: FAQ: Why is Spacemacs hanging on startup?
  ;;   https://github.com/syl20bnr/spacemacs/blob/master/doc/FAQ.org
  (setq tramp-ssh-controlmaster-options
        (concat "-o ControlPath='tramp.%%C' "
                "-o ControlMaster=auto "
                "-o ControlPersist=no"))

  ;; Set the customization file path; must be done here in `user-init'.
  ;; See: https://github.com/syl20bnr/spacemacs/commit/6212795
  (setq custom-file "~/.spacemacs.d/custom.el")

  ;; END: `dotspacemacs/user-init'
  )


(defun dotspacemacs/user-config ()
  "Configuration function for user code.
  This function is called at the very end of Spacemacs initialization after
  layers configuration. You are free to put any user code."
  ;;
  (setq-default tab-width 8)
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

  (with-eval-after-load 'whitespace
    ;; Make whitespace visible by enabling the `whitespace-mode'
    ;; Credit: http://ergoemacs.org/emacs/whitespace-mode.html
    ;; Make `whitespace-mode' use just basic colors
    (setq whitespace-style
          '(face tabs trailing lines-tail indentation
                spaces space-before-tab space-after-tab newline
                tab-mark newline-mark))
    ;; Set the marks used for different whitespace characters
    (setq whitespace-display-mappings
          '((space-mark   ?\     [?·]     [?.])  ; space - middle dot
            (space-mark   ?\xA0  [?¤]     [?_])  ; hard space - currency sign
            (newline-mark ?\n    [?↵ ?\n]  [?$ ?\n])  ; eol - downwards arrow
            ;(newline-mark ?\n    [?$ ?\n])  ; eol - dollar sign
            ;(newline-mark ?\n    [?¶ ?\n]  [?$ ?\n])  ; eol - pilcrow
            ;(newline-mark ?\n    [?¬ ?\n]  [?$ ?\n])  ; eol - negation
            ;; WARNING: the mapping below has a problem:
            ;; When a TAB occupies exactly one column, it will display the
            ;; character ?\xBB at that column followed by a TAB which goes
            ;; to the next TAB column.
            ;(tab-mark     ?\t    [?» ?\t] [?\\ ?\t])  ; tab - right guillemet
            ))
    ;; Set the faces for different whitespace characters
    (set-face-attribute 'whitespace-space-before-tab nil
                        :underline t
                        :background nil
                        :foreground (face-attribute 'font-lock-warning-face
                                                    :foreground))
    (set-face-attribute 'whitespace-space-after-tab nil
                        :underline t
                        :background nil
                        :foreground (face-attribute 'font-lock-warning-face
                                                    :foreground))
    ;; Highlight the line part that goes beyond `whitespace-line-column'
    (setq whitespace-line-column fill-column)
    ;; END 'whitespace
    )
  (global-whitespace-mode)

  ;; Indentation settings
  (setq-default
   js2-basic-offset 4
   js-indent-level 4
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   )

  (add-hook 'c-mode-common-hook
            (lambda ()
              (setq indent-tabs-mode t)
              (c-set-style "bsd")))

  ;; Python
  (setq python-shell-interpreter "python3")

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

  ;; Change `powerline' separator
  (setq powerline-default-separator 'slant)
  ;; Display tildes in the fringe on empty lines
  (global-vi-tilde-fringe-mode)

  ;; Configure BibTeX completion for `helm-bibtex'
  (setq bibtex-completion-bibliography '("~/papers/references.bib"))
  (setq bibtex-completion-library-path "~/papers/")  ;; where to find PDFs
  (setq bibtex-completion-notes-path "~/papers/notes.org")
  (setq bibtex-completion-pdf-field "File")
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (start-process "xdg-open" "*helm-bibtex-open*" "xdg-open" fpath)))
  (setq bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${=type=:3} ${year:4} ${author:36} ${title:*}")))
  ;; Configure the BibTeX file for `org-ref'
  (setq org-ref-default-bibliography '("~/papers/references.bib")
        org-ref-pdf-directory "~/papers/"
        org-ref-bibliography-notes "~/papers/notes.org")

  ;; C++ mode
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-gcc-language-standard "c++14"
                    flycheck-clang-language-standard "c++14")))

  ;; ESS: Turn off the automatic replacement of `_' by `<-'
  (add-hook 'ess-mode-hook (lambda () (ess-toggle-underscore nil)))

  ;; Set font only after the first GUI frame initialized, to workaround the
  ;; error when Emacs starts in daemon mode.
  (spacemacs|do-after-display-system-init
   ;; Set the font of `variable-pitch'
   (set-face-font 'variable-pitch "M+ 1C")
   ;; Set monospaced font size for Chinese (from `chinese' layer)
   (spacemacs//set-monospaced-font "M+ 1mn" "WenQuanYi Zen Hei" 14 14)
   )
  ;;
  ;; Load custom configurations
  (push "~/.spacemacs.d/config" load-path)
  (push "~/.spacemacs.d/local" load-path)
  (require 'my-crypt)
  (require 'my-org)
  (require 'my-mu4e)
  (require 'my-calendar)

  ;; Load customization file
  (if (file-readable-p custom-file)
      (load-file custom-file))

  ;; END: `dotspacemacs/user-config'
  )


;;; init.el ends here
