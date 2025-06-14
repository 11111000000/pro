;;; про-мониторы.el --- Настройка второго и третьего мониторов -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exwm-randr)

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "eDP-1")
(defvar имя-внешнего-монитора "HDMI-1")
(defvar имя-третьего-монитора "DP-1")

(defun применить-расположение-мониторов ()
  "Применить расположение мониторов из переменной `расположение-монитора`.

Настроить экраны при любом изменении конфигурации мониторов."
  (let ((process-connection-type nil))
    (start-process-shell-command
     "xrandr" nil
     (concat "xrandr "
            " --output " имя-встроенного-монитора " --auto --pos 0x0 --rotate normal "
            " --output " имя-внешнего-монитора " --auto --above " имя-встроенного-монитора " --rotate normal "
            (if имя-третьего-монитора (concat " --output " имя-третьего-монитора " --auto --rotate normal --above "  имя-внешнего-монитора) "")))
    ;; Wait for xrandr to complete, then refresh EXWM
    (run-with-timer 2 nil #'exwm-randr-refresh)))


(defun про-мониторы-инициализировать ()
  "Инициализация exwm-randr и применение конфигурации мониторов."
  (setq exwm-randr-workspace-monitor-plist
        (list 0 имя-встроенного-монитора 1 имя-внешнего-монитора 2 имя-третьего-монитора))
  (exwm-randr-mode t)
  ;; Применять каждый раз при изменении конфигурации мониторов
  (add-hook 'exwm-randr-screen-change-hook #'применить-расположение-мониторов)
  ;; Применить расположение мониторов единожды при инициализации
  (применить-расположение-мониторов))

(provide 'про-мониторы)
;;; про-мониторы.el ends here
