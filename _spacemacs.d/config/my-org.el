;;; my-org.el --- Custom Configurations for Org Mode
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; Created: 2016-05-31
;;

;;; Commentary:
;; Custom configurations for Emacs Org mode
;;
;; [1] Spacemacs: Org layer
;;     https://github.com/syl20bnr/spacemacs/blob/master/layers/org/README.org
;; [2] Norang: Org Mode (by Bernt Hansen)
;;     http://doc.norang.ca/org-mode.html
;;

;;; Code:

;; Default task to clock in whenever the clock normally stops
(defvar my/organization-task-id "bb7f1326-bda4-11e6-a30d-185e0f33a428")

;; Allow single character alphabetical bullets
;; Need to be set before `org.el' is loaded
(setq org-list-allow-alphabetical t)

;; NOTE:
;; Spacemacs use the `org' from the ELPA instead of the one shipped with
;; Emacs.  Therefore, any `org'-related code should NOT be loaded before
;; `dotspacemacs/user-config'.
(with-eval-after-load 'org
  (require 'org-habit)
  (require 'org-clock)
  (require 'ox-latex)

  ;; Load custom helper functions for Org
  (require 'my-org-helpers)

  ;; Default location to look for Org files
  (setq org-directory '("~/org"))
  ;; Directories of files / files to be used for agenda display
  (setq org-agenda-files '("~/org"))
  ;; Default file for storing notes, also the fallback file for capture
  (setq org-default-notes-file "~/org/refile.org")

  ;; TODO state keywords and face settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|"
                    "CANCELLED(c@/!)" "MAIL" "PHONE" "MEETING")))
  ;(setq org-todo-keyword-faces
  ;      '(("TODO"      :foreground "red"          :weight bold)
  ;        ("NEXT"      :foreground "blue"         :weight bold)
  ;        ("DONE"      :foreground "forest green" :weight bold)
  ;        ("WAITING"   :foreground "orange"       :weight bold)
  ;        ("HOLD"      :foreground "magenta"      :weight bold)
  ;        ("CANCELLED" :foreground "forest green" :weight bold)
  ;        ("MAIL"      :foreground "forest green" :weight bold)
  ;        ("PHONE"     :foreground "forest green" :weight bold)
  ;        ("MEETING"   :foreground "forest green" :weight bold)))
  ;; Automatically assign tags on state changes
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))  ;; add "CANCELLED" tag
          ("WAITING" ("WAITING" . t))  ;; add "WAITING" tag
          ("HOLD" ("WAITING") ("HOLD" . t))
          (done ("WAITING") ("HOLD"))  ;; remove "WAITING" and "HOLD" tags
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  ;; Allow to select a state while bypass the associated logging
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Task" entry (file "")
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("n" "Note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
          ("d" "Diary" entry (file+datetree "diary.org")
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("p" "Phone Call" entry (file "")
           "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
          ("m" "Meeting" entry (file "")
           "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
          ("h" "Habit" entry (file "")
           "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
          ))

  ;; Remove empty `LOGBOOK' drawers on clock out
  (add-hook 'org-clock-out-hook 'my/remove-empty-drawer-on-clock-out 'append)

  ;; More handy shortcuts
  (global-set-key (kbd "<f9>") 'org-agenda)

  ;; Exclude `DONE' state tasks from refile targets
  (setq org-refile-target-verify-function 'my/verify-refile-target)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)
  ;; Compact the block agenda view
  (setq org-agenda-compact-blocks t)
  ;; Custom agenda command definitions
  (setq org-agenda-custom-commands
        '(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("h" "Habits" tags-todo "STYLE=\"habit\""
           ((org-agenda-overriding-header "Habits")
            (org-agenda-sorting-strategy
             '(todo-state-down effort-up category-keep))))
          (" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (org-tags-match-list-sublevels nil)))
            (tags-todo "-CANCELLED/!"
                       ((org-agenda-overriding-header "Stuck Projects")
                        (org-agenda-skip-function 'my/skip-non-stuck-projects)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-HOLD-CANCELLED/!"
                       ((org-agenda-overriding-header "Projects")
                        (org-agenda-skip-function 'my/skip-non-projects)
                        (org-tags-match-list-sublevels 'indented)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED/!NEXT"
                       ((org-agenda-overriding-header
                         (concat "Project Next Tasks"
                                 (if my/hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'my/skip-projects-and-habits-and-single-tasks)
                        (org-tags-match-list-sublevels t)
                        (org-agenda-todo-ignore-scheduled my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Project Subtasks"
                                 (if my/hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'my/skip-non-project-tasks)
                        (org-agenda-todo-ignore-scheduled my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Standalone Tasks"
                                 (if my/hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'my/skip-project-tasks)
                        (org-agenda-todo-ignore-scheduled my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-with-date my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-CANCELLED+WAITING|HOLD/!"
                       ((org-agenda-overriding-header
                         (concat "Waiting and Postponed Tasks"
                                 (if my/hide-scheduled-and-waiting-next-tasks
                                     ""
                                   " (including WAITING and SCHEDULED tasks)")))
                        (org-agenda-skip-function 'my/skip-non-tasks)
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-todo-ignore-scheduled my/hide-scheduled-and-waiting-next-tasks)
                        (org-agenda-todo-ignore-deadlines my/hide-scheduled-and-waiting-next-tasks)))
            (tags "-REFILE/"
                  ((org-agenda-overriding-header "Tasks to Archive")
                   (org-agenda-skip-function 'my/skip-non-archivable-tasks)
                   (org-tags-match-list-sublevels nil))))
           nil)))

  (defun my/org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with `/ RET'"
    (and (cond
          ((string= tag "hold") t)  ; exclude "HOLD" tasks
         )
         (concat "-" tag)))
  (setq org-agenda-auto-exclude-function 'my/org-auto-exclude-function)

  ;; Clocking
  ;;
  ;; Resume clocking task when Emacs is restarted
  (org-clock-persistence-insinuate)
  ;; Save the running clock and all clock history when exiting Emacs,
  ;; and load it on startup
  (setq org-clock-persist t)
  ;; Do not prompt to resume an active clock
  (setq org-clock-persist-query-resume nil)
  ;; Enable auto clock resolution for finding open clocks
  (setq org-clock-auto-clock-resolution 'when-no-clock-is-running)
  ;; Show lot of clocking history so it's easy to pick items off the list
  (setq org-clock-history-length 23)
  ;; Resume clocking task on clock-in if the clock is open
  (setq org-clock-in-resume t)
  ;; Change tasks to `NEXT' when clocking in
  (setq org-clock-in-switch-to-state 'my/clock-in-to-next)
  ;; Separate drawers for clocking and logs
  (setq org-drawers '("PROPERTIES" "LOGBOOK"))
  ;; Save clock data and state changes and notes in the `LOGBOOK' drawer
  (setq org-clock-into-drawer t)
  ;; Remove clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)
  ;; Clock out when moving task to a done state
  (setq org-clock-out-when-done t)
  (add-hook 'org-clock-out-hook 'my/clock-out-maybe 'append)
  ;; Use discrete minute intervals (no rounding) increments for time editing
  (setq org-time-stamp-rounding-minutes '(1 1))

  ;; Agenda clock report parameters
  (setq org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
  ;; Set default column view headings: Task Effort ClockSummary
  (setq org-columns-default-format
        "%80ITEM(task) %10Effort(Effort){:} %10CLOCKSUM")
  ;; Global `Effort' estimate values,
  ;; and global `STYLE' property values for completion
  (setq org-global-properties
        '(("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
          ("STYLE_ALL" . "habit")))
  ;; Agenda log mode items to display (closed and state changes by default)
  (setq org-agenda-log-mode-items '(closed state))

  ;; Tags with fast selection keys
  (setq org-tag-alist
        '((:startgroup)
          ("@office" . ?o)
          ("@home" . ?H)
          ("@dorm" . ?d)
          (:endgroup)
          ("WAITING" . ?w)
          ("HOLD" . ?h)
          ("PERSONAL" . ?P)
          ("WORK" . ?W)
          ("ORG" . ?O)
          ("ASTRO" . ?a)
          ("NOTE" . ?n)
          ("CANCELLED" . ?c)
          ("FLAGGED" . ??)))
  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))
  ;; For tag searches ignore tasks with scheduled and deadline dates
  (setq org-agenda-tags-todo-honor-ignore-options t)

  ;; Any task with a subtask using a todo keyword is a project.
  ;; Projects are "stuck" if they have no subtask with a `NEXT' todo keyword.

  ;; Start the agenda overview on the current day
  (setq org-agenda-start-on-weekday nil)
  ;; Show week's agenda (default)
  (setq org-agenda-span 'week)
  ;; Disable the default stuck projects agenda view
  (setq org-stuck-projects '("" nil nil ""))

  ;; Archive
  (setq org-archive-mark-done nil)
  (setq org-archive-location "%s_archive::* Archived Tasks")

  ;; Enable in-using Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (python . t)
     (ditaa . t)
     ))

  ;; Enable `org-indent-mode' by default at startup
  (setq org-startup-indented t)

  ;; Set the bullet symbols for `org-bullets'
  (setq org-bullets-bullet-list '("♠" "♥" "♣" "♦"))  ;; "SHoCkeD" ordering

  ;; Set the path to the `ditaa' program
  (setq org-ditaa-jar-path "~/.spacemacs.d/local/ditaa/ditaa.jar")

  ;; END: Org-mode configurations
  )


(provide 'my-org)

;;; my-org.el ends here
