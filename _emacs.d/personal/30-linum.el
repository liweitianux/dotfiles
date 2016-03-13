;;; ~/.emacs.d/personal/30-linum.el --- Configure linum-mode
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credits:
;; [1] https://stackoverflow.com/a/3879664/4856091
;; [2] https://github.com/redguardtoo/emacs.d - lisp/init-linum-mode.el
;;
;; Aaron LI
;; 2016-02-24
;;

;;; Commentary:
;; Configure linum-mode for Emacs.

;;; Code:

;; Turn on `linum-mode' globally
(global-linum-mode t)

;; Separte line numbers from text
(setq linum-format "%4d\u2502")

;; Inhibit `linum-mode' for following specified modes
(setq linum-mode-inhibit-modes-list '(eshell-mode
                                      shell-mode
                                      dictionary-mode
                                      erc-mode
                                      browse-kill-ring-mode
                                      Buffer-menu-mode
                                      etags-select-mode
                                      dired-mode
                                      help-mode
                                      ;text-mode
                                      fundamental-mode
                                      jabber-roster-mode
                                      jabber-chat-mode
                                      inferior-js-mode
                                      inferior-python-mode
                                      inferior-scheme-mode
                                      twittering-mode
                                      compilation-mode
                                      weibo-timeline-mode
                                      woman-mode
                                      Info-mode
                                      calc-mode
                                      calc-trail-mode
                                      comint-mode
                                      inf-ruby-mode
                                      gud-mode
                                      org-mode
                                      vc-git-log-edit-mode
                                      log-edit-mode
                                      term-mode
                                      w3m-mode
                                      speedbar-mode
                                      mu4e-main-mode
                                      mu4e-headers-mode
                                      mu4e-view-mode
                                      mu4e-compose-mode
                                      gnus-group-mode
                                      gnus-summary-mode
                                      gnus-article-mode
                                      calendar-mode))
(defadvice linum-on (around linum-on-inhibit-for-modes)
           "Stop the load of linum-mode for some major modes."
           (unless (member major-mode linum-mode-inhibit-modes-list)
             ad-do-it))
(ad-activate 'linum-on)

