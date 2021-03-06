;;; my-mu4e.el --- mu4e (mu for Emacs) configurations
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; Created: 2016-02-21
;;

;;; Commentary:
;; mu4e (mu for Emacs) configurations for spacemacs.
;; mu (maildir indexer/searcher): https://github.com/djcb/mu
;;

;;; Code:

;; maildir location
(setq mu4e-maildir "~/mail")

;; turn on debug: log debug information to the *mu4e-log* buffer
(setq mu4e-debug t)

(with-eval-after-load 'mu4e
  ;; speed up indexing
  (setq mu4e-index-cleanup nil)  ; don't do a full cleanup check
  (setq mu4e-index-lazy-check t)  ; don't consider up-to-date directories

  ;; multiple email accounts: contexts (require mu4e >= 0.9.16)
  (setq mu4e-contexts
     `( ,(make-mu4e-context
          :name "aly"
          :enter-func (lambda ()
                        (mu4e-message "Switched to context: aly"))
          ;; `leave-func' not defined
          ;; `match-func' is invoked just before `mu4e-compose-pre-hook'
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg :to '("aly@aaronly.me"
                                     "aly@liwt.net"))))
          :vars '((user-mail-address      . "aly@aaronly.me")
                  (user-full-name         . "Aaron LI")
                  (mu4e-drafts-folder     . "/aly/drafts")
                  (mu4e-trash-folder      . "/aly/trash")
                  (mu4e-refile-folder     . "/aly/archive")
                  (mu4e-sent-folder       . "/aly/sent")
                  (mu4e-compose-signature . "Aaron")
                  ;; copy message to sent folder
                  (mu4e-sent-messages-behavior . sent)))
        ,(make-mu4e-context
          :name "weitian"
          :enter-func (lambda ()
                        (mu4e-message "Switched to context: weitian"))
          ;; `leave-func' not defined
          ;; `match-func' is invoked just before `mu4e-compose-pre-hook'
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches
                           msg :to '("wt@liwt.net"
                                     "weitian@liwt.net"
                                     "weitian@aaronly.me"))))
          :vars '((user-mail-address      . "wt@liwt.net")
                  (user-full-name         . "Weitian LI")
                  (mu4e-drafts-folder     . "/wt/drafts")
                  (mu4e-trash-folder      . "/wt/trash")
                  (mu4e-refile-folder     . "/wt/archive")
                  (mu4e-sent-folder       . "/wt/sent")
                  (mu4e-compose-signature . "Weitian")
                  ;; copy message to sent folder
                  (mu4e-sent-messages-behavior . sent)))
        ,(make-mu4e-context
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
                  (mu4e-drafts-folder     . "/outlook-aly/drafts")
                  (mu4e-trash-folder      . "/outlook-aly/trash")
                  (mu4e-refile-folder     . "/outlook-aly/archive")
                  (mu4e-sent-folder       . "/local-sent")
                  (mu4e-compose-signature . "Aly")
                  (mu4e-sent-messages-behavior . sent)))
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
                  (mu4e-drafts-folder     . "/gmail-aly/drafts")
                  (mu4e-trash-folder      . "/gmail-aly/trash")
                  (mu4e-refile-folder     . "/gmail-aly/archive")
                  (mu4e-sent-folder       . "/local-sent")
                  (mu4e-compose-signature . "Aly")
                  ;; save message to local sent folder for safety
                  (mu4e-sent-messages-behavior . sent)))
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
                  (mu4e-drafts-folder     . "/outlook-li/drafts")
                  (mu4e-trash-folder      . "/outlook-li/trash")
                  (mu4e-refile-folder     . "/outlook-li/archive")
                  (mu4e-sent-folder       . "/local-sent")
                  (mu4e-compose-signature . "Weitian")
                  (mu4e-sent-messages-behavior . sent)))
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
                  (mu4e-drafts-folder     . "/gmail-li/drafts")
                  (mu4e-trash-folder      . "/gmail-li/trash")
                  (mu4e-refile-folder     . "/gmail-li/archive")
                  (mu4e-sent-folder       . "/local-sent")
                  (mu4e-compose-signature . "Weitian")
                  (mu4e-sent-messages-behavior . sent)))
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
                  (mu4e-drafts-folder     . "/sjtu/drafts")
                  (mu4e-trash-folder      . "/sjtu/trash")
                  (mu4e-refile-folder     . "/sjtu/archive")
                  (mu4e-sent-folder       . "/sjtu/sent")
                  (mu4e-compose-signature .
                    (concat "Weitian LI\n"
                            "Shanghai Jiao Tong University"))
                  ;; copy message to sent folder
                  (mu4e-sent-messages-behavior . sent)))
        ,(make-mu4e-context
          :name "Autistici"
          :enter-func (lambda ()
                        (mu4e-message "Switched to context: autistici"))
          ;; `leave-func' not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg
                            :to "liweitianux@autistici.org")))
          :vars '((user-mail-address      . "liweitianux@autistici.org")
                  (user-full-name         . "Weitian LI")
                  (mu4e-drafts-folder     . "/autistici/drafts")
                  (mu4e-trash-folder      . "/autistici/trash")
                  (mu4e-refile-folder     . "/autistici/archive")
                  (mu4e-sent-folder       . "/autistici/sent")
                  (mu4e-compose-signature . "Weitian")
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
                  (mu4e-drafts-folder     . "/foxmail/drafts")
                  (mu4e-trash-folder      . "/foxmail/trash")
                  (mu4e-refile-folder     . "/foxmail/archive")
                  (mu4e-sent-folder       . "/local-sent")
                  (mu4e-compose-signature . "Weitian")
                  (mu4e-sent-messages-behavior . sent)))))

  ;; start with the first (default) context
  ;; default: `ask-if-none' (ask when there's no context yet, and none match)
  (setq mu4e-context-policy 'pick-first)
  ;; compose with the current context if no context matches (default: `ask')
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

  ;; Allow mu4e to fetch mail and update index
  ;; NOTE: I use own cron/shell task to get/sync email
  (setq mu4e-get-mail-command "true")
  ;; Get mail and update index periodically in the background (unit: seconds)
  (setq mu4e-update-interval 300)

  ;; set shortcuts for frequent mail folders (also used by moving messages)
  ;; NOTE: do not use shortcut `o' as it is used for `[o]ther'
  (setq mu4e-maildir-shortcuts
        '(("/important"         . ?i)
          ("/archive"           . ?a)
          ("/gmail-aly/archive" . ?G)
          ("/gmail-li/archive"  . ?g)
          ("/outlook-aly/inbox" . ?l)
          ("/outlook-li/inbox"  . ?L)
          ("/sjtu/inbox"        . ?s)))

  ;; This works better with 'mbsync'
  (setq mu4e-change-filenames-when-moving t)

  ;; kill the buffer after sending a message
  (setq message-kill-buffer-on-exit t)

  ;; compose messages in a separate frame
  (setq mu4e-compose-in-new-frame t)

  ;; exclude myself when replying to all
  (setq mu4e-compose-dont-reply-to-self t)

  ;; Apply `format=flowed' to outgoing messages, which transform the message
  ;; into the proper format at the time of sending. (mu4e >= 0.9.17)
  ;; Then each paragraph should be written as a long line, or use `M-q' to
  ;; reformat the paragraph into a single line.
  (setq mu4e-compose-format-flowed t)

  ;; Do not cleanup trailing whitespaces
  (add-hook 'mu4e-compose-mode-hook
            'spacemacs/toggle-whitespace-cleanup-off)

  ;; enable inline images
  (setq mu4e-view-show-images t)
  ;; use imagemagick if available
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; display rich-text messages
  (require 'mu4e-contrib)
  ;; use `shr' html renderer; reqiure emacs >= 24.4
  (require 'shr)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (add-hook 'mu4e-view-mode-hook
            (lambda ()
              ;; try to emulate some of the `eww' key-bindings
              (local-set-key (kbd "<tab>")     'shr-next-link)
              (local-set-key (kbd "<backtab>") 'shr-previous-link)))
  ;; change the luminosity form dark theme
  (setq shr-color-visible-luminance-min 70)

  ;; Set bookmarks
  (setq mu4e-bookmarks
        '(("flag:unread AND NOT flag:trashed" "INBOX"                ?b)
          ("flag:draft"                       "Drafts"               ?d)
          ("tag:todo OR tag:task"             "TODO / Task"          ?t)
          ("tag:work"                         "Work"                 ?W)
          ("date:today..now"                  "Today's messages"     ?T)
          ("date:7d..now"                     "Last 7 days"          ?w)
          ("date:1m..now"                     "Last month"           ?m)
          ("flag:flagged"                     "Flagged messages"     ?f)
          ("tag:astro"                        "Astro"                ?a)
          ("tag:arxiv OR from:arxiv.org"      "arXiv"                ?x)
          ("tag:SMS"                          "SMS"                  ?S)
          ("flag:trashed OR tag:\\\\Trash"    "Deleted"              ?D)
          ("mime:image/*"                     "Messages with images" ?p)))

  ;; Headers list appearance
  (setq mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-headers-fields '((:date       . 18)  ;; also :human-date
                              (:flags      .  5)
                              (:from-or-to . 22)
                              (:subject    . nil)))  ;; also :thread-subject

  ;; header fields to be shown in the view buffer
  (add-to-list 'mu4e-view-fields :size t)
  (add-to-list 'mu4e-view-fields :user-agent t)

  ;; use fancy non-ascii characters in various places
  ;(setq mu4e-use-fancy-chars t)

  ;; show full addresses instead of just names when view message
  (setq mu4e-view-show-addresses t)

  ;; also include messages related to the searched messages
  (setq mu4e-headers-include-related t)
  ;; exclude the duplicate messages with the same `Message-ID'
  ;(setq mu4e-headers-skip-duplicates t)

  ;; Customize the reply/quote citation format
  (setq message-citation-line-format "On %a, %Y-%b-%d at %R %Z, %f wrote:\n")
  ;; Choose to use the above formatted string
  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  ;; confirm before sending
  (add-hook 'message-send-hook
            (lambda ()
              (unless (yes-or-no-p "Sure you want to send this?")
                (signal 'quit nil))))

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
  (defun my/search-for-sender (msg)
    "Search for messages sent by the sender of the message at point."
    (mu4e-headers-search
    (concat "from:" (cdar (mu4e-message-field msg :from)))))
  (add-to-list 'mu4e-view-actions
               '("xSearch for sender" . my/search-for-sender) t)

  ;; set `mu4e' as emacs' default email program
  (setq mail-user-agent 'mu4e-user-agent)

  ;; END: with-eval-after-load 'mu4e
  )


;;; Key bindings for Evil
;; Credits:
;; * https://github.com/philc/emacs-config
;;   .emacs.d/elisp/mu4e-mode-personal.el
;; * https://github.com/JorisE/evil-mu4e
;; * Guide for using Emacs with Evil
;;   https://github.com/noctuid/evil-guide
;;

(with-eval-after-load 'mu4e
  (require 'evil)
  (require 'evil-leader)

  ;; Set all mu4e modes (except for `mu4e-compose-mode') to start in
  ;; `evil-motion-state'.
  ;; NOTE: The `evil-leader' by default does not work in `evil-motion-state'!
  ;;       (see: https://github.com/cofi/evil-leader/issues/1 )
  (dolist (mode '(mu4e-main-mode
                  mu4e-headers-mode
                  mu4e-view-mode
                  mu4e-org-mode))
    (evil-set-initial-state mode 'motion))
  ;; Set `mu4e-compose-mode' to start in `evil-normal-state'
  (evil-set-initial-state 'mu4e-compose-mode 'normal)

  ;; mu4e main mode
  ;;
  ;; Copy the original `mu4e-main-mode-map' and give it precedence over
  ;; evil's default `motion' keymap, therefore, we only need to define
  ;; several necessary keys as done below.
  ;; See: https://github.com/syl20bnr/spacemacs/wiki/Keymaps-guide
  ;;
  (evil-make-overriding-map mu4e-main-mode-map 'motion t)
  ;;
  ;; Override the above copied `mu4e-main-mode-map'
  (evil-define-key 'motion mu4e-main-mode-map
    (kbd "TAB") 'evil-forward-paragraph
    "0"  'evil-beginning-of-line
    "$"  'evil-end-of-line
    "J"  'mu4e~headers-jump-to-maildir
    "j"  'evil-next-line
    "k"  'evil-previous-line)

  ;; mu4e headers mode
  (evil-make-overriding-map mu4e-headers-mode-map 'motion t)
  (evil-define-key 'motion mu4e-headers-mode-map
    "gg" 'evil-goto-first-line
    "gl" 'mu4e-show-log
    "gr" 'mu4e-headers-rerun-search
    "J"  'mu4e~headers-jump-to-maildir
    "j"  'evil-next-line
    "k"  'evil-previous-line
    "0"  'evil-beginning-of-line
    "$"  'evil-end-of-line
    "v"  'evil-visual-line)

  ;; mu4e view mode
  (evil-make-overriding-map mu4e-view-mode-map 'motion t)
  (evil-define-key 'motion mu4e-view-mode-map
    "gg"   'evil-goto-first-line
    "gl"   'mu4e-show-log
    "gu"   'mu4e-view-go-to-url
    "\C-j" 'mu4e-view-headers-next
    "\C-k" 'mu4e-view-headers-prev
    "H"    'mu4e-view-toggle-html
    "j"    'evil-next-line
    "k"    'evil-previous-line
    "0"    'evil-beginning-of-line
    "$"    'evil-end-of-line
    "v"    'evil-visual-char
    "V"    'evil-visual-line
    "y"    'evil-yank)

  ;; mu4e compose mode (start in `evil-normal-state')
  (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
  (evil-leader/set-key-for-mode 'mu4e-compose-mode
    "," 'message-send-and-exit
    "c" 'message-send-and-exit
    "d" 'message-dont-send
    "k" 'mu4e-message-kill-buffer
    "a" 'mml-attach-file
    "s" 'mml-secure-message-sign
    "e" 'mml-secure-message-encrypt
    ";" 'mu4e-context-switch)

  ;; END: with-eval-after-load 'mu4e
  )


(provide 'my-mu4e)

;;; my-mu4e.el ends here
