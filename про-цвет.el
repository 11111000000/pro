;;; про-цвет.el --- EMACS и Пустота
;;; Commentary:

;; /AST не имеет ни цвета ни вкуса ни запаха, ни - физических размера или веса, какой-либо формы - только структуру/
;; 
;; Цвета должны подчёркивать структуру текста, не отвлекая от него.
;; Стандартные типографские методы - /курсив/, *толщина*, яркость, разнообразные шрифты, лигатуры - арсенал средств
;; оформления неожиданно богаче палитры средней цветовой темы...
;; Но цвета бывают и полезны, для конкретных целей, например для подсветки уровней вложенности скобок либо форм, разных идентификаторов, ошибок )

;;; Code:

(defun tao-palette ()
  "Палитра."
  (tao-theme-yang-palette))

(require 'установить-из)

(use-package tao-theme
  :if window-system
  :init (установить-из :repo "11111000000/tao-theme-emacs")
  :custom ((tao-theme-use-height t)
          (tao-theme-use-boxes t)
          (tao-theme-use-sepia nil)
                                        ;(tao-theme-scale-fn '(lambda ()'(3 5 8 10 23 37 60 97 158 195 218 232 241 246 250 252 259)))
          )
  :config
  (require 'tao-yang-theme)
  (load-theme 'tao-yang t)
  (загрузить 'face-remap))

(provide 'про-цвет)
;;; цвет.el ends here
