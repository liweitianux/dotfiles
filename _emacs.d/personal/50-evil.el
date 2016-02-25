;;; ~/.emacs.d/personal/50-evil.el --- Custom configurations for Evil
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credit:
;; [1] https://github.com/redguardtoo/emacs.d - lisp/init-evil.el
;; [2] http://stackoverflow.com/a/10166400
;;
;; Aaron LI
;; 2016-02-21
;;

;;; Commentary:
;; Custom configurations for Evil.

;;; Code:

;; ESC *always* quits
(define-key minibuffer-local-map            [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map         [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map    [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'keyboard-quit)

;; Prefer Emacs way after pressing ":" in evil-mode
(define-key evil-ex-completion-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
(define-key evil-ex-completion-map (kbd "M-p") 'previous-complete-history-element)
(define-key evil-ex-completion-map (kbd "M-n") 'next-complete-history-element)

;; Evil keybindings for `org-mode'
;; As a general RULE, mode specific evil leader keys started with
;; uppercased character or 'g' or special character except "=" and "-"
(evil-declare-key 'normal org-mode-map
  "gh" 'outline-up-heading
  "gl" 'outline-next-visible-heading
  "ga" 'org-agenda
  "S"  'org-store-link
  "H"  'org-beginning-of-line  ; smarter behaviour on headlines etc.
  "L"  'org-end-of-line  ; smarter behaviour on headlines etc.
  "$"  'org-end-of-line  ; smarter behaviour on headlines etc.
  "^"  'org-beginning-of-line  ; ditto
  "<"  'org-metaleft  ; out-dent
  ">"  'org-metaright  ; indent
  (kbd "TAB") 'org-cycle)

;; evil-escape: Escape from anything with a customizable key sequence
;; https://github.com/syl20bnr/evil-escape
;(prelude-require-package 'evil-escape)
;(require 'evil-escape)
;(setq-default evil-escape-key-sequence "kj")
;(setq evil-escape-exclude-major-modes '(dired-mode))
;(evil-escape-mode 1)
;(global-set-key [escape] 'evil-escape)

;; evil-leader
;; https://github.com/cofi/evil-leader
(prelude-require-package 'evil-leader)
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(evil-leader/set-key
  ;; {{{ only usable in GUI
  "=" 'increase-default-font-height
  "-" 'decrease-default-font-height
  ;; }}}
  "bf" 'buffer-menu
  "mu" 'mu4e
  "rm" 'remember
  "rn" 'remember-notes)

;; evil-matchit: jump between matched tags in Emacs
;; https://github.com/redguardtoo/evil-matchit
(prelude-require-package 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

