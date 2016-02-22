;;; ~/.emacs.d/personal/fonts.el --- Fonts configurations for Emacs.
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credits:
;; [1] http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; [2] http://zhuoqiang.me/a/torture-emacs
;;
;; Aaron LI
;; 2016-02-22
;;

;;; Commentary:
;; English and Chinese fonts configurations for Emacs.

;;; Code:

;; To increase the Unicode coverage of your favorite programming font
;; https://github.com/cpitclaudel/monospacifier
;(dolist (ft (fontset-list))
;  (set-fontset-font ft 'unicode
;                    (font-spec :name "Inconsolata"))
;  (set-fontset-font ft 'unicode
;                    (font-spec :name "Symbola monospacified for Inconsolata")
;                    nil 'append))

;; English font
;; change the default font for the current frame, as well as future frames
;(set-face-attribute 'default nil :font "Terminus:pixelsize=16")
(set-face-attribute 'default nil :font "Inconsolata:pixelsize=14")

;; Chinese font
;; available charset: kana han symbol cjk-misc bopomofo
;; font-spec:
;;   `:size': either a non-negative integer that specifies the pixel size,
;;            or a floating-point number that specifies the point size.
(dolist (charset '(kana han cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "WenQuanYi Zen Hei" :size 14)))
;; rescale chinese font to match: 1 Chinese = 2 English characters
;; (however, this will cause that the Chinese characters are higher)
;(setq face-font-rescale-alist '(("WenQuanYi Bitmap Song" . 1.2)))

