;;; ~/.emacs.d/personal/10-prelude-config.el --- Custom configurations for Prelude
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; 2016-02-21
;;

;;; Commentary:
;; Custom configurations for the `Prelude' Emacs kit.

;;; Code:

;; Disable `key-chord-mode' for `evil-mode'
;; (or just do not enable the `prelude-key-chord' module)
(key-chord-mode -1)

;; Do NOT automatically clean whitespaces on save
;(setq prelude-clean-whitespace-on-save nil)

;; Disable `flyspell-mode': do not spellcheck on the fly
;(setq prelude-flyspell nil)

;; Set default TeX engine
(setq-default TeX-engine 'xetex)

