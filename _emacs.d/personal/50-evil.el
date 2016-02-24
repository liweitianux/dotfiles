;;; ~/.emacs.d/personal/50-evil.el --- Custom configurations for Evil
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credit:
;; [1] http://stackoverflow.com/a/10166400
;;
;; Aaron LI
;; 2016-02-21
;;

;;; Commentary:
;; Custom configurations for Evil.

;;; Code:

;;; ESC ALWAYS quits
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;; evil-leader
;; https://github.com/cofi/evil-leader
(prelude-require-package 'evil-leader)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; evil-matchit: jump between matched tags in Emacs
;; https://github.com/redguardtoo/evil-matchit
(prelude-require-package 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

