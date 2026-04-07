;;; про-цвет.el --- Цветовая схема Tao Yang для Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: theme, color, tao, palette, syntax-highlighting
;; URL: https://github.com/username/emacs.d/blob/main/среда/про-цвет.el
;;
;;; Commentary:
;;
;; Этот файл настраивает минималистичную цветовую схему Tao Yang, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Цвета должны подчёркивать структуру текста, не отвлекая
;; от него. Tao Yang использует типографские методы (курсив, толщина, яркость)
;; для оформления, а цвета — только для конкретных целей: подсветка скобок,
;; форм, идентификаторов, ошибок. Это создаёт спокойную и продуктивную среду.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Загрузка темы
;;  2. Определение палитры
;;  3. Финал: Provide и ends here
;;
;; Использование: (load-theme 'tao-yang t) или подключается автоматически.
;; Рекомендуется загружать как можно раньше, до use-package.
;;
;;; Code:

;; КРИТИЧЕСКИ ВАЖНО: Вызываем как можно раньше, ДО use-package, чтобы не мигал фон.
(condition-case nil
    (load-theme 'tao-yang t)
  (error)) ;; если вдруг пакет не установлен — не падаем, просто продолжим

(require 'загрузить)

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
           ;;(tao-theme-scale-fn '(lambda ()'(10 12 13 15 23 37 60 97 158 195 218 232 241 246 250 252 259)))
           )
  :config
  (load-theme 'tao-yang t)
  (загрузить 'face-remap))

(provide 'про-цвет)
;;; про-цвет.el ends here
