;;; my-calendar.el --- Custom calendar configurations
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; Created: 2017-03-01
;; Updated: 2017-03-14
;;

;;; Commentary:
;; Custom calendar configurations
;;

;;; Code:

(with-eval-after-load 'calendar
  (require 'cal-china)

  ;; Date style: month/day/year (default)
  (setq calendar-date-style 'american)

  ;; Wrapper function of `diary-chinese-anniversary' using an intuitive year
  ;; NOTE: Built-in `diary-chinese-anniversary' accepts an obscure year
  ;;       notation consisting of the 60-year cycle number and year number
  ;;       within this cycle.
  ;; Credit: https://emacs-china.org/t/agenda-list/2119/15
  (defun my/diary-chinese-anniversary (month day &optional year mark)
    "Like `diary-chinese-anniversary' but use an intuitive year argument."
    (if year
        (let* ((d-date (diary-make-date month day year))
               (a-date (calendar-absolute-from-gregorian d-date))
               (c-date (calendar-chinese-from-absolute a-date))
               (cycle (car c-date))
               (yy (cadr c-date))
               (y (+ (* 100 cycle) yy)))
          (diary-chinese-anniversary month day y mark))
      (diary-chinese-anniversary month day year mark)))

  ;; END: `with-eval-after-load'
  )

(provide 'my-calendar)

;;; my-calendar.el ends here
