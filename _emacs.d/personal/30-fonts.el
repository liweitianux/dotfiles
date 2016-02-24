;;; ~/.emacs.d/personal/30-fonts.el --- Fonts configurations for Emacs.
;;
;; -*- mode: emacs-lisp -*-
;; vim: set ts=8 sw=2 tw=0 fenc=utf-8 ft=lisp:
;;
;; Credits:
;; [1] http://baohaojun.github.io/perfect-emacs-chinese-font.html
;; [2] http://zhuoqiang.me/a/torture-emacs
;; [3] https://github.com/syl20bnr/spacemacs - layers/chinese/config.el
;; [4] https://gist.github.com/exaos/4493582 - el-emacs-zh-face.el
;; [5] https://github.com/cpitclaudel/monospacifier
;;
;; Aaron LI
;; 2016-02-22
;;

;;; Commentary:
;; English and Chinese fonts configurations for Emacs.

;;; 字体显示测试
;;
;; 中英文对齐
;;-------1---------2---------3---------4---------5---------6---------7--
;; abab  abababab  abababab  abababab  abababababab
;; 你我  你我你我  你我你我  你我你我  你我你我你我
;;3456789+123456789+123456789+123456789+123456789+123456789+123456789+12
;; 半角： 0 o O 1 l I | i ; : . ~ \ / - _ = ! @ # $ % ^ & * ` ' " () [] {}
;; 全角：  ， ； 、 。 ？ ！
;; —— “ ” ‘ ’ 《 》 ［ ］ 「」『』〈〉《》〖〗【】〔〕
;;---------------------------------------------------------------
;; 这儿的字符至少应该显示正常！
;; Esperanto: ĉ Ĉ ĝ Ĝ ĥ Ĥ ĵ Ĵ ŝ Ŝ ŭ Ŭ -- Ĵaudo Ĥoro aĝo antaŭ ĝoja
;; 化学元素： 𨧀 dù, 𨨏 (钅波) bō ㄅㄛ 𨭆 hēi 䥑 鐽 dá ㄉㄚˊ鎶
;; IPA: ðɫŋɹɾʃθtʒæɑəəɚɜɛɝɪɪ̈ɒɔʊʊ̈ʌ
;; àáâãäåæç èéêë ìíîï ðñòóôõö øùúûüýþÿ ÀÁÂÃÄÅ
;; Ç ÈÉÊË ÌÍÎÏ ÐÑ ÒÓÔÕÖ ØÙÚÛÜÝÞß
;; ¢ € ₠ £ ¥ ¤ ° © ® ™ § ¶ † ‡ ※ •◦ ‣ ✓●■◆○□◇★☆♠♣♥♦♤♧♡♢
;; ←→↑↓↔↖↗↙↘⇐⇒⇑⇓⇔⇗⇦⇨⇧⇩ ↞↠↟↡ ↺↻ ☞☜☝☟ ∀¬∧∨∃⊦∵∴∅∈∉⊂⊃⊆⊇⊄⋂⋃
;; ♩♪♫♬♭♮♯  ➀➁➂➃➄➅➆➇➈➉ 卐卍✝✚✡☥⎈☭☪☮☺☹ ☯☰☱☲☳☴☵☶☷
;; ☠☢☣☤♲♳⌬♨♿ ☉☼☾☽ ♀♂ ♔♕♖ ♗♘♙ ♚♛ ♜♝♞♟
;;
;; 查看某个字符是什么字体，使用函数: (describe-char); 默认绑定: C-u C-x =

;;; Code:

(defun my-make-font-string (font-name font-size)
  "Font string examples: 'name:pixelsize=14', 'name-size'."
  (if (and (stringp font-size)
           (equal ":" (string (elt font-size 0))))
      (format "%s%s" font-name font-size)
      (format "%s-%s" font-name font-size)))

(defun my-set-fonts (english-font
                    english-font-size
                    chinese-font
                    &optional unicode-font)
  "The english-font-size could be set to ':pixelsize=14' or an integer."
  ;; Set the default English font: for most latin characters
  (message "Set English font to: %s" english-font)
  (set-face-attribute 'default nil
                      :font (my-make-font-string english-font
                                                 english-font-size))

  ;; Set Chinese font
  (message "Set Chinese font to: %s" chinese-font)
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
                      charset
                      (font-spec :family chinese-font)))

  ;; Set the fallback Unicode font
  (when unicode-font
    (message "Set Unicode fallback font to: %s" unicode-font)
    (dolist (ft (fontset-list))
      (set-fontset-font t 'unicode (font-spec :family english-font))
      (set-fontset-font t 'unicode (font-spec :family unicode-font)
                        nil 'append))))

;; Uniformly rescale the Chinese and Unicode characters
(setq face-font-rescale-alist
      '(("WenQuanYi Zen Hei" . 1.2)))

;; Only set fonts when Eamcs running in graphic mode
(when (display-graphic-p)
  (my-set-fonts "Inconsolata" ":pixelsize=14"
                "WenQuanYi Zen Hei"
                "Symbola monospacified for Inconsolata"))

;; Set fonts for new frame of `emacsclient'
(add-hook 'after-make-frame-functions
  (lambda (new-frame)
    (select-frame new-frame)
    (if (display-graphic-p)
      (my-set-fonts "Inconsolata" ":pixelsize=14"
                    "WenQuanYi Zen Hei"
                    "Symbola monospacified for Inconsolata"))))

