;;; про-мониторы.el --- Настройка второго и третьего мониторов -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exwm-randr)

(defgroup про-мониторы nil
  "Настройка и управление конфигурацией мониторов под EXWM."
  :group 'exwm)

(defcustom про/monitor-refresh-delay 0.2
  "Сколько секунд ждать после вызова xrandr,
прежде чем отправлять `exwm-randr-refresh`."
  :type 'number
  :group 'про-мониторы)

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "eDP-1")
(defvar имя-внешнего-монитора "HDMI-1")
(defvar имя-третьего-монитора "DP-1")
(defvar про/monitor-refresh-timer nil)

(defun применить-расположение-мониторов ()
  "Применить расположение мониторов из переменной `расположение-монитора`.

Настроить экраны при любом изменении конфигурации мониторов."
  (let* ((cmd
          (concat "xrandr "
                  " --output " имя-встроенного-монитора " --auto --pos 0x0 --rotate normal "
                  " --output " имя-внешнего-монитора " --auto --above " имя-встроенного-монитора " --rotate normal "
                  (if имя-третьего-монитора
                      (concat " --output " имя-третьего-монитора " --auto --rotate normal --above "  имя-внешнего-монитора)
                    ""))))
    ;; Запускаем xrandr синхронно, чтобы геометрия успела примениться до refresh
    (call-process-shell-command cmd)
    ;; Даём X немного времени «устаканиться»
    (sleep-for про/monitor-refresh-delay)
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
              (when про/monitor-refresh-timer
                (cancel-timer про/monitor-refresh-timer))
              (setq про/monitor-refresh-timer
                    (run-at-time 0.2 nil #'применить-расположение-мониторов)))))

(provide 'про-мониторы)
;;; про-мониторы.el ends here
