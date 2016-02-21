;;;
;;; ~/.emacs.d/prelude-modules.el
;;; copyed from `sample/prelude-modules.el'
;;;
;;; Aaron LI
;;; 2016-02-21
;;;

;;; Uncomment the modules you'd like to use and restart Prelude afterwards

;; Emacs IRC client
;;(require 'prelude-erc)
;; Super charges Emacs completion for C-x C-f and more
(require 'prelude-ido)
;; Interface for narrowing and search
(require 'prelude-helm)
;; Enable Helm everywhere
(require 'prelude-helm-everywhere)
(require 'prelude-company)
;; Binds useful features to key combinations (NOTE: conflict with `evil-mode')
;;(require 'prelude-key-chord)
;; (require 'prelude-mediawiki)
;; Evil is an extensible vi layer for Emacs
(require 'prelude-evil)

;;; Programming languages support
(require 'prelude-c)
;;(require 'prelude-clojure)
;;(require 'prelude-coffee)
;;(require 'prelude-common-lisp)
;;(require 'prelude-css)
(require 'prelude-emacs-lisp)
;;(require 'prelude-erlang)
;;(require 'prelude-elixir)
;;(require 'prelude-go)
;;(require 'prelude-haskell)
(require 'prelude-js)
(require 'prelude-latex)
(require 'prelude-lisp)
;;(require 'prelude-ocaml)
;; Org-mode helps you keep TODO lists, notes and more
(require 'prelude-org)
;;(require 'prelude-perl)
(require 'prelude-python)
;;(require 'prelude-ruby)
;;(require 'prelude-scala)
(require 'prelude-scheme)
(require 'prelude-shell)
;;(require 'prelude-scss)
;; Emacs mode for web templates
;;(require 'prelude-web)
(require 'prelude-xml)
;;(require 'prelude-yaml)

