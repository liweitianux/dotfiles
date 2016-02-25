;;; ~/.emacs.d/personal/10-config.el --- Custom Emacs configurations
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
;; Custom Emacs configurations.

;;; Code:

;; Turn off the scroll bar
(scroll-bar-mode -1)

;; Show the `*scratch*' buffer directly with splash
(setq inhibit-startup-screen t)
(setq initial-major-mode 'emacs-lisp-mode)

;; Prevent kill and yank commands from accessing the clipboard
(setq x-select-enable-clipboard nil)

;; Zero out default splitting thresholds
(setq split-height-threshold 0
      split-width-threshold 0)

;; Backup and auto save settings
;; Credit: https://www.emacswiki.org/emacs/AutoSave
(defvar my-backup-dir (expand-file-name "backup/" prelude-dir))
(defvar my-autosave-dir (expand-file-name "autosave/" prelude-dir))
;; Place the backup files in the specified directory under `prelude-dir'
(setq backup-by-copying t  ;; do not clobber symlinks
      backup-directory-alist `((".*" . ,my-backup-dir))
      version-control t  ;; use versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)
;; Place both auto-saved files and auto-save-list file in the same directory
(setq auto-save-list-file-prefix my-autosave-dir
      auto-save-file-name-transforms `((".*" ,my-autosave-dir t)))

(setq-default tab-width 4)
(setq-default fill-column 78)

;; Auto-indent with the Return key
(define-key global-map (kbd "RET") 'newline-and-indent)

