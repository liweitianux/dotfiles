;;; ~/.emacs.d/personal/mu4e.el --- mu4e (mu for Emacs) configurations
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; 2016-02-21
;;

;;; Commentary:
;; mu4e (mu for Emacs) configurations
;; mu (maildir indexer/searcher): https://github.com/djcb/mu

;;; Code:

;; mu4e package path
(add-to-list 'load-path "~/local/emacs/site-lisp/mu4e")
(require 'mu4e)

;; turn on debug: log debug information to the *mu4e-log* buffer
(setq mu4e-debug t)

(setq mu4e-maildir "~/mail")

;; multiple email accounts: contexts (require mu4e >= 0.9.16)
(setq mu4e-contexts
  `( ,(make-mu4e-context
        :name "Outlook-aly"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: outlook-aly"))
        ;; `leave-func' not defined
        ;; `match-func' is invoked just before `mu4e-compose-pre-hook'
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to "aaronly.me@outlook.com")))
        :vars '((user-mail-address      . "aaronly.me@outlook.com")
                (user-full-name         . "Aaron LI")
                (mu4e-sent-folder       . "/outlook-aly/sent")
                (mu4e-drafts-folder     . "/outlook-aly/drafts")
                (mu4e-trash-folder      . "/outlook-aly/trash")
                (mu4e-refile-folder     . "/outlook-aly/archive")
                (mu4e-compose-signature . "Aly")
                (mu4e-sent-messages-behavior . delete)))
      ,(make-mu4e-context
        :name "Gmail-aly"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: gmail-aly"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to "aaronly.me@gmail.com")))
        :vars '((user-mail-address      . "aaronly.me@gmail.com")
                (user-full-name         . "Aaron LI")
                ;; special folders
                (mu4e-sent-folder       . "/sent")
                (mu4e-drafts-folder     . "/gmail-aly/drafts")
                (mu4e-trash-folder      . "/gmail-aly/trash")
                (mu4e-refile-folder     . "/gmail-aly/archive")
                (mu4e-compose-signature . "Aly")
                ;; do NOT save message to 'sent-folder'
                (mu4e-sent-messages-behavior . delete)))
      ,(make-mu4e-context
        :name "outlook-li"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: outlook-li"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to '("liweitianux@live.com"
                                "liweitianux@outlook.com"))))
        :vars '((user-mail-address      . "liweitianux@live.com")
                (user-full-name         . "Weitian LI")
                (mu4e-sent-folder       . "/outlook-li/sent")
                (mu4e-drafts-folder     . "/outlook-li/drafts")
                (mu4e-trash-folder      . "/outlook-li/trash")
                (mu4e-refile-folder     . "/outlook-li/archive")
                (mu4e-compose-signature . "Weitian LI")
                (mu4e-sent-messages-behavior . delete)))
      ,(make-mu4e-context
        :name "gmail-li"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: gmail-li"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to "liweitianux@gmail.com")))
        :vars '((user-mail-address      . "liweitianux@gmail.com")
                (user-full-name         . "Weitian LI")
                (mu4e-sent-folder       . "/sent")
                (mu4e-drafts-folder     . "/gmail-li/drafts")
                (mu4e-trash-folder      . "/gmail-li/trash")
                (mu4e-refile-folder     . "/gmail-li/archive")
                (mu4e-compose-signature . "Weitian LI")
                (mu4e-sent-messages-behavior . delete)))
      ,(make-mu4e-context
        :name "sjtu"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: sjtu"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to "liweitianux@sjtu.edu.cn")))
        :vars '((user-mail-address      . "liweitianux@sjtu.edu.cn")
                (user-full-name         . "Weitian LI")
                (mu4e-sent-folder       . "/sjtu/sent")
                (mu4e-drafts-folder     . "/sjtu/drafts")
                (mu4e-trash-folder      . "/sjtu/trash")
                (mu4e-refile-folder     . "/sjtu/archive")
                (mu4e-compose-signature .
                  (concat "Weitian LI\n"
                          "Department of Physics and Astronomy\n"
                          "Shanghai Jiao Tong University"))
                ;; copy message to sent folder
                (mu4e-sent-messages-behavior . sent)))
      ,(make-mu4e-context
        :name "autistici"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: autistici"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to "liweitianux@autistici.org")))
        :vars '((user-mail-address      . "liweitianux@autistici.org")
                (user-full-name         . "Weitian LI")
                (mu4e-sent-folder       . "/autistici/sent")
                (mu4e-drafts-folder     . "/autistici/drafts")
                (mu4e-trash-folder      . "/autistici/trash")
                (mu4e-refile-folder     . "/autistici/archive")
                (mu4e-compose-signature . "Weitian LI")
                ;; copy message to sent folder
                (mu4e-sent-messages-behavior . sent)))
      ,(make-mu4e-context
        :name "foxmail"
        :enter-func (lambda ()
                      (mu4e-message "Switched to context: foxmail"))
        ;; `leave-func' not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg
                          :to '("liweitianux@foxmail.com"
                                "527922216@qq.com"))))
        :vars '((user-mail-address      . "liweitianux@foxmail.com")
                (user-full-name         . "Weitian LI")
                (mu4e-sent-folder       . "/foxmail/sent")
                (mu4e-drafts-folder     . "/foxmail/drafts")
                (mu4e-trash-folder      . "/foxmail/trash")
                (mu4e-refile-folder     . "/foxmail/archive")
                (mu4e-compose-signature . "Weitian LI")
                (mu4e-sent-messages-behavior . delete)))))

;; start with the first (default) context
;; default: `ask-if-none' (ask when there's no context yet, and none match)
(setq mu4e-context-policy 'pick-first)
;; compose with the current context if no context matches
;; default: `ask'
(setq mu4e-compose-context-policy nil)

;; auto construct addresses list by extracting addresses from contexts
(setq mu4e-user-mail-address-list
      (delq nil
            (mapcar (lambda (context)
                      (when (mu4e-context-vars context)
                        (cdr (assq 'user-mail-address
                                   (mu4e-context-vars context)))))
                    mu4e-contexts)))

;; send mail using `msmtp'
(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-evil t
      ;; choose the account according to the `From' header
      message-sendmail-extra-arguments '("--read-envelope-from")
      sendmail-program "msmtp")

;; allow for fetch mail and update index using 'U' in the main view
(setq mu4e-get-mail-command "offlineimap -o -1")
;(setq mu4e-get-mail-command "true" nil nil
;      "Disable fetching mail as it is done by a daemon")

;; frequent mail folders, with shortcuts
;; do not use shortcut `o' as it is used for `[o]ther'
(setq mu4e-maildir-shortcuts
      '(("/important"         . ?i)
        ("/archive"           . ?a)
        ("/gmail-aly/archive" . ?G)
        ("/gmail-li/archive"  . ?g)
        ("/outlook-aly/inbox" . ?l)
        ("/outlook-li/inbox"  . ?L)
        ("/sjtu/inbox"        . ?s)))

;; do not keep message buffers around
(setq message-kill-buffer-on-exit t)

;; enable inline images
(setq mu4e-view-show-images t)
;; use imagemagick if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; display rich-text messages
(require 'mu4e-contrib)
;; use `shr' html renderer; reqiure emacs >= 24.4
(setq mu4e-html2text-command 'mu4e-shr2text)
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            ;; try to emulate some of the `eww' key-bindings
            (local-set-key (kbd "<tab>")     'shr-next-link)
            (local-set-key (kbd "<backtab>") 'shr-previous-link)))
;; change the luminosity form dark theme
(setq shr-color-visible-luminance-min 70)

;; customize bookmarks
(defvar mu4e-bookmarks
  '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
    ("date:today..now"                  "Today's messages" ?t)
    ("date:7d..now"                     "Last 7 days" ?w)
    ("flag:flagged"                     "Flagged messages" ?f)
    ("tag:astro"                        "Astro" ?a)
    ("tag:arxiv"                        "arXiv" ?x)
    ("mime:image/*"                     "Messages with images" ?p)))

;; headers list appearance
(setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
      mu4e-headers-fields '((:date       . 18) ;; also :human-date
                            (:flags      .  5)
                            (:from-or-to . 22)
                            (:subject    . nil))) ;; also :thread-subject

;; use fancy non-ascii characters in various places
;(setq mu4e-use-fancy-chars t)

;; show full addresses instead of just names when view message
(setq mu4e-view-show-addresses t)

;; also include messages related to the searched messages
(setq mu4e-headers-include-related t)
;; exclude the duplicate messages with the same `Message-ID'
(setq mu4e-headers-skip-duplicates t)

;; confirm before sending
(add-hook 'message-send-hook
          (lambda ()
            (unless (yes-or-no-p "Sure you want to send this?")
              (signal 'quit nil))))

;; apply "format=flowed" to outgoing messages, enabling receiving clients
;; that supports this feature to reflow the paragraphs. (from mu4e FAQ)
(add-hook 'mu4e-compose-mode-hook
          (defun cpb-compose-setup ()
            "Outgoing mails get `format=flowed'."
            (use-hard-newlines t 'guess)))

;; custom actions
;; retag action
(setq mu4e-action-tags-header "X-Keywords")
(add-to-list 'mu4e-headers-actions
             '("tRetag message" . mu4e-action-retag-message) t)
(add-to-list 'mu4e-view-actions
             '("tRetag message" . mu4e-action-retag-message) t)
;; view in browser
(add-to-list 'mu4e-view-actions
             '("bView in browser" . mu4e-action-view-in-browser) t)
;; search for messages by the sender of the opened message
(defun search-for-sender (msg)
  "Search for messages sent by the sender of the message at point."
  (mu4e-headers-search
   (concat "from:" (cdar (mu4e-message-field msg :from)))))
(add-to-list 'mu4e-view-actions
             '("xSearch for sender" . search-for-sender) t)

;; set `mu4e' as emacs' default email program
(setq mail-user-agent 'mu4e-user-agent)
