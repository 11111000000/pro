;;; про-мониторы.el --- Настройка второго и третьего мониторов -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exwm-randr)

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "eDP-1")
(defvar имя-внешнего-монитора "HDMI-1")
(defvar имя-третьего-монитора "DP-1")

(defun применить-расположение-мониторов ()
  "Применить расположение мониторов из переменной `расположение-монитора`."
  (let ()
    (start-process-shell-command
     "xrandr" nil
     (concat "xrandr "
            " --output " имя-встроенного-монитора " --auto --pos 0x0 --rotate normal "
            " --output " имя-внешнего-монитора " --auto --above " имя-встроенного-монитора " --rotate left "
            (if имя-третьего-монитора (concat " --output " имя-третьего-монитора " --auto --rotate normal --above "  имя-внешнего-монитора) "")))
    (sit-for 1)
    (exwm-randr-refresh)))

(eval-after-load 'exwm
  '(progn
     (setq exwm-randr-workspace-monitor-plist
           (list 0 имя-встроенного-монитора 1 имя-внешнего-монитора 2 имя-третьего-монитора))
     (exwm-randr-mode t)
     (add-hook 'exwm-init-hook (lambda ()
                                (применить-расположение-мониторов)))
     (add-hook 'exwm-randr-screen-change-hook
               'применить-расположение-мониторов)))

(provide 'про-мониторы)
;;; про-мониторы.el ends here
