;;; aly-org-config.el --- My configurations for org-mode
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; Created: 2016-05-31
;;

;;; Commentary:
;; My custom configurations for the `org' of spacemacs.
;; https://github.com/syl20bnr/spacemacs/blob/master/layers/org/README.org
;;

;;; Code:

;; NOTE:
;; Spacemacs use the `org' from the ELPA instead of the one shipped with
;; Emacs.  Then, any `org'-related code should NOT be loaded before
;; `dotspacemacs/user-config'.
(with-eval-after-load 'org
  (setq org-agenda-files '("~/org"))
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sh . t)
     (python . t)
     (ditaa . t)
     ))
  ;; set the path to the `ditaa' program
  (setq org-ditaa-jar-path "~/.spacemacs.d/local/ditaa/ditaa.jar")
  )  ;; with-eval-after-load 'org


(provide 'aly-org-config)

;;; aly-org-config.el ends here
