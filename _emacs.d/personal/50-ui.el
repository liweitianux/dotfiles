;;; ~/.emacs.d/personal/50-ui.el --- UI configurations depending on packages
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credits:
;; [1] https://github.com/wasamasa/dotemacs/blob/master/init.org
;; [2] From Vim to Emacs+Evil chaotic migration guide
;;     http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;;
;; Aaron LI
;; 2016-02-24
;;

;;; Commentary:
;; UI configurations for Emacs that depends on 3rd-party packages.

;;; Code:

;; powerline: Emacs version of the Vim powerline
(prelude-require-package 'powerline)
(require 'powerline)
(powerline-vim-theme)

;; fill-column-indicator
;; https://www.emacswiki.org/emacs/FillColumnIndicator
(prelude-require-package 'fill-column-indicator)
(require 'fill-column-indicator)
;; Enabled `fci-mode' in text and programming modes,
;; but not special buffers, dired, shell, etc.
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda ()
    (if (and
          (not (string-match "^\*.*\*$" (buffer-name)))
          (not (eq major-mode 'dired-mode)))
        (fci-mode 1))))
;(global-fci-mode 1)

