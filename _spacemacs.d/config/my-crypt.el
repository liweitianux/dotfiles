;;; my-crypt.el --- Custom sign/encrypt configurations
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Aaron LI
;; Created: 2017-10-15
;;

;;; Commentary:
;; Custom sign/encrypt configurations
;;

;;; Code:

;; Default Encrypt
;; https://www.emacswiki.org/emacs/DefaultEncrypt
;; https://www.emacswiki.org/emacs/download/jl-encrypt.el
(require 'jl-encrypt)

;; Ask for the signing key
(setq mm-sign-option 'guided)

;; Use message sender to find the OpenPGP key to sign with
(setq mml-secure-openpgp-sign-with-sender t)

;; Also encrypt to the message sender, otherwise, the send message
;; *cannot* be decrypted by the sender!
(setq mml-secure-openpgp-encrypt-to-self t)


(provide 'my-crypt)

;;; my-crypt.el ends here
