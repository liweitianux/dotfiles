;;; ~/.emacs.d/personal/fcitx.el --- Configurations for fcitx.el
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; 2016-02-22
;;

;;; Commentary:
;; Configurations for fcitx.el (https://github.com/cute-jumper/fcitx.el)

;;; Code:

;; fcitx.el: make fcitx better in Emacs.
(prelude-require-package 'fcitx)

(require 'fcitx)

;; disable fcitx by prefix keys
(fcitx-prefix-keys-setup)  ;; default: `C-x' and `C-c'
(fcitx-prefix-keys-turn-on)

;; evil support
(fcitx-evil-turn-on)

;; disable fcitx when use `M-x'
(fcitx-M-x-turn-on)
;; disable fcitx when use `M-!' or `M-&'
(fcitx-shell-command-turn-on)
;; disable fcitx when use `M-:'
(fcitx-eval-expression-turn-on)

