;;; -*- mode: emacs-lisp -*-
;;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;;
;;; Org-mode configurations
;;;
;;; Credit:
;;; [1] http://www.john2x.com/emacs.html
;;;
;;; Aaron LI
;;; 2016-02-21
;;;

(setq org-directory "~/org")
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "LATER" "|" "DONE" "DELEGATED")))
;; default notes file for `org-capture'
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; set custom `org-capture' templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Other")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+datetree (concat org-directory "/notes.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))
;; add custom `org-agenda' to show:
;;   * agenda for the week
;;   * things currently working on
;;   * list of remaining TODO items
(setq org-agenda-custom-commands
      '(("z" "Agenda and Tasks"
         ((agenda "")
          (todo   "DOING")
          (todo   "TODO")))))
;; enable font-locking for org source blocks
(setq org-src-fontify-natively t)
;; do not evaulate source blocks when exporting
(setq org-export-babel-evaluate nil)
;;
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
