;;; ~/.emacs.d/personal/preload/theme.el --- Override Prelude theme
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; 2016-02-23
;;

;;; Commentary:
;; Override Prelude theme (default: zenburn)

;;; Code:

;; manually load `prelude-packages', in order to use `prelude-require-package'
(require 'prelude-packages)

(prelude-require-package 'monokai-theme)
;; override the default theme of Prelude
(setq prelude-theme 'monokai)

