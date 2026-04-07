;;; про-буферы.el --- Управление буферами в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: buffer, ibuffer, uniquify, auto-revert
;; URL: https://github.com/username/emacs.d/blob/main/интерфейс/про-буферы.el
;;
;;; Commentary:
;;
;; Этот файл настраивает управление буферами в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Буфер — базовая сущность Emacs, в которой находится
;; всё остальное: файлы, окна приложений и т.д. Здесь мы добавляем
;; уникальные имена буферов, автообновление, группировку через ibuffer.
;; Это делает работу с множеством файлов удобной и организованной.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Уникальные имена буферов
;;  2. Автообновление буферов
;;  3. Ibuffer с группировкой
;;  4. Утилиты для буферов
;;  5. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-буферы) в init.el.
;; Рекомендуется подключать после про-внешний-вид.
;;
;;; Code:
;;;; Буферы

;; Каждый буфер называется уникально

(setq uniquify-buffer-name-style 'forward)

;; иногда нужно обновить буфер немедленно

(defun обновить-буфер-немедленно ()
    "ничего не спрашивая, обновить буфер."
    (interactive)
    (revert-buffer t t))

;; но восновном буферы актуализируются автоматически

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; асинхронные буферы скрыты из списка

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; новые асинхронные буферы переименовываются не спрашивая ничего:

(setq-default async-shell-command-buffer 'rename-buffer)

;; буфер с ошибками показывается только при ошибках

(require 'warnings)

(setq warning-minimum-level :error)

;;;; Группировать по проекту

(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :hook (ibuffer . my/ibuffer-projectile-groups)
  :config
  (defun my/ibuffer-projectile-groups ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(provide 'про-буферы)
;;; про-буферы.el ends here
