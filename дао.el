;;; package --- Summary
;;; Commentary:
;;; Code:
;;; EMACS и Пустота

;; /AST не имеет ни цвета ни вкуса ни запаха, ни - физических размера или веса, какой-либо формы - только структуру/

;; Цвета должны подчёркивать структуру текста, не отвлекая от него.  Стандартные типографские методы - /курсив/, *толщина*, яркость, разнообразные шрифты, лигатуры - арсенал средств оформления неожиданно богаче палитры средней цветовой темы...

;; Но цвета бывают и полезны, для конкретных целей ( например - подсветки уровней вложенности скобок либо форм, разных идентификаторов, ошибок )

(setq-default color-theme-is-cumulative nil)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun tao-palette () "tao-palette"
       (tao-theme-yang-palette))

(use-package tao-theme
  :straight '(tao-theme :host github :repo "11111000000/tao-theme-emacs")
  :if window-system
  :custom ((tao-theme-use-height t)
           (tao-theme-use-boxes t) 
           (tao-theme-use-sepia nil)
           (tao-theme-scale-fn '(lambda ()'(3 5 8 10 23 37 60 97 158 195 218 232 241 246 250 252 259)))
           )
  :init
  ;; (add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-lisp/tao-theme-emacs")
  (load-theme 'tao-yang 't)
  (require 'face-remap))

(provide 'дао)
;;; дао.el ends here
