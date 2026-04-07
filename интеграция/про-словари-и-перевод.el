;;; про-словари-и-перевод.el --- Словари и переводчики в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: dictionary, translate, go-translate
;; URL: https://github.com/username/emacs.d/blob/main/интеграция/про-словари-и-перевод.el
;;
;;; Commentary:
;;
;; Этот файл настраивает словари и переводчики в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Перевод — важная часть работы с иностранными
;; текстами. Здесь настраивается go-translate для быстрого перевода
;; прямо в Emacs с поддержкой нескольких движков (Google, Bing).
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Настройка go-translate
;;  2. Переводчики и движки
;;  3. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-словари-и-перевод) в init.el.
;; Перевод: M-x gt-translator
;;
;;; Code:

;; (use-package go-translate
;;   :ensure t
;;   :functions (gt-translator
;;               gt-prompt-picker
;;               gt-bing-engine
;;               gt-buffer-render
;;               gt-google-engine
;;               gt-taker)
;;   :defines (gt-default-translator)
;;   :custom ((gt-translate-list '(("en" "ru")
;;                                 ("ru" "en"))))
;;   :config
;;   (setq gt-default-translator
;;         (gt-translator
;;          :taker (gt-taker
;;                  :langs '(en ru))
;;          :engines (list (gt-google-engine) (gt-bing-engine))
;;          :render (gt-buffer-render))))

(use-package translat
  :load-path "~/Code/translat-el/")


(provide 'про-словари-и-перевод)
;;; про-словари-и-перевод.el ends here
