;;; ~/.emacs.d/personal/10-config.el --- Custom Emacs configurations
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credits:
;; [1] https://github.com/wasamasa/dotemacs/blob/master/init.org
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

;; Change the initial buffer to `*Remember*' instead of `*scratch*'
(setq initial-buffer-choice 'remember-notes)
(setq remember-notes-initial-major-mode 'org-mode)
;; Disable the `C-c C-c' keybinding of `remember-notes-mode-map' to avoid
;; the conflict with `org-mode'
(with-eval-after-load 'remember
                      (define-key remember-notes-mode-map (kbd "C-c C-c") nil))

;; Prevent kill and yank commands from accessing the clipboard
(setq x-select-enable-clipboard nil)

(setq fill-column 80)

;; Zero out default splitting thresholds
(setq split-height-threshold 0
      split-width-threshold 0)

;; Backup and auto save settings
;; Credit: https://www.emacswiki.org/emacs/AutoSave
(defvar prelude-backup-dir (expand-file-name "backup/" prelude-dir))
(defvar prelude-autosave-dir (expand-file-name "autosave/" prelude-dir))
;; Place the backup files in the specified directory under `prelude-dir'
(setq backup-by-copying t  ;; do not clobber symlinks
      backup-directory-alist `((".*" . ,prelude-backup-dir))
      version-control t  ;; use versioned backups
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)
;; Place both auto-saved files and auto-save-list file in the same directory
(setq auto-save-file-name-transforms `((".*" ,prelude-autosave-dir t))
      auto-save-list-file-prefix prelude-autosave-dir)

(setq-default tab-width 4)

