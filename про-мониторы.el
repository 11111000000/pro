;;; про-мониторы.el --- Настройка второго монитора -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'exwm-randr)

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "eDP-1")
(defvar имя-внешнего-монитора "HDMI-1")

(defun применить-расположение-монитора ()
  "Применить расположение монитора из переменной `расположение-монитора`."
  (let ((xrandr-position (cond
                         ((eq расположение-монитора 'сверху) "--below")
                         ((eq расположение-монитора 'справа) "--left-of")
                         (t "--left-of"))))
    (start-process-shell-command
     "xrandr" nil
     (concat "xrandr --output " имя-внешнего-монитора " --auto --rotate normal --output " имя-встроенного-монитора " --auto " xrandr-position " " имя-внешнего-монитора " --primary"))
    (sit-for 1)
    (exwm-randr-refresh)))

(eval-after-load 'exwm
  '(progn
     (setq exwm-randr-workspace-monitor-plist
          (list 0 имя-встроенного-монитора 1 имя-внешнего-монитора))
     (exwm-randr-mode t)
     (add-hook 'exwm-randr-screen-change-hook
              'применить-расположение-монитора)))

(provide 'про-мониторы)
;;; про-мониторы.el ends here
