;;; про-мониторы.el --- Настройка мультимониторной конфигурации -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: monitor, xrandr, exwm, multi-monitor
;; URL: https://github.com/username/emacs.d/blob/main/среда/про-мониторы.el
;;
;;; Commentary:
;;
;; Этот файл настраивает мультимониторную конфигурацию в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? При работе с несколькими мониторами важно правильно
;; настроить их расположение и привязку workspace в EXWM. Здесь мы
;; используем xrandr для определения мониторов и настраиваем задержку
;; для корректной работы с EXWM.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Настройка группы и переменных
;;  2. Функции для мониторов
;;  3. Интеграция с EXWM
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-мониторы) в init.el.
;; Требует exwm-randr и xrandr.
;;
;;; Code:

(require 'exwm-randr)

(defgroup про-мониторы nil
  "Настройка и управление конфигурацией мониторов под EXWM."
  :group 'exwm)

(defcustom pro/monitor-refresh-delay 0.2
  "Сколько секунд ждать после вызова xrandr,
прежде чем отправлять `exwm-randr-refresh`."
  :type 'number
  :group 'про-мониторы)

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "DP-1")
(defvar имя-внешнего-монитора "DP-2")
(defvar имя-третьего-монитора nil)
(defvar pro/monitor-refresh-timer nil)

(defun применить-расположение-мониторов ()
  "Применить расположение мониторов из переменной `расположение-монитора`.

Настроить экраны при любом изменении конфигурации мониторов."
  (let* ((cmd
          (concat "xrandr "
                  " --output " имя-встроенного-монитора " --auto --pos 0x0 --rotate normal "
                  " --output " имя-внешнего-монитора " --auto --right-of " имя-встроенного-монитора " --rotate left "
                  (if имя-третьего-монитора
                      (concat " --output " имя-третьего-монитора " --auto --rotate normal --above "  имя-внешнего-монитора)
                    ""))))
    ;; Запускаем xrandr синхронно, чтобы геометрия успела примениться до refresh
    (call-process-shell-command cmd)
    ;; Даём X немного времени «устаканиться»
    (sleep-for pro/monitor-refresh-delay)
    ;; Делаем refresh только если EXWM уже загружен и RandR-режим активен
    (when (and (featurep 'exwm)
               (fboundp 'exwm-randr-refresh)
               (bound-and-true-p exwm-randr-mode))
      (exwm-randr-refresh))))


(defun про-мониторы-инициализировать ()
  "Только установка workspace<->monitor привязки и хук пересборки xrandr.
exwm-randr-mode и xrandr запускаются вне этой функции!"
  (setq exwm-randr-workspace-monitor-plist
        (list 0 имя-встроенного-монитора
              1 имя-внешнего-монитора
              2 имя-третьего-монитора))
  ;; Смена топологии при изменении состава мониторов — дебаунс, чтобы не мигало на старте
  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (when pro/monitor-refresh-timer
                (cancel-timer pro/monitor-refresh-timer))
              (setq pro/monitor-refresh-timer
                    (run-at-time 0.2 nil #'применить-расположение-мониторов)))))

(provide 'про-мониторы)
;;; про-мониторы.el ends here
