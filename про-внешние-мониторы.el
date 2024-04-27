;;; про-внешние-мониторы.el --- Настройка второго монитора
;;; Commentary:
;;; Code:

(defvar расположение-монитора 'сверху)
(defvar имя-встроенного-монитора "eDP-1")
(defvar имя-внешнего-монитора "DP-3")

(eval-after-load 'exwm
  '(progn
     (require 'exwm-randr)

     (setq exwm-randr-workspace-output-plist (list 0 имя-встроенного-монитора 1 имя-внешнего-монитора))
                                        ;     (setq exwm-randr-workspace-output-plist '(0 "LVDS-1" 1 "VGA-1"))

     (defun vga-справа ()
       "Monitor on top with normal orientation."
       (start-process-shell-command
        "xrandr" nil "xrandr --output VGA-1 --auto --rotate normal --output LVDS-1 --auto --left-of VGA-1 --primary")
       (exwm-randr-refresh))

     (defun vga-справа-повёрнут ()
       "Monitor on top with normal orientation."
       (start-process-shell-command
        "xrandr" nil "xrandr --output VGA-1 --auto --rotate left --output LVDS-1 --auto --left-of VGA-1 --primary")
       (exwm-randr-refresh))

     (defun dp-3-справа-повёрнут ()
       "Monitor on top rotated."
       (start-process-shell-command
        "xrandr" nil "xrandr --output DP-3 --auto --rotate left --output eDP-1 --auto --left-of DP-3 --primary")
       (exwm-randr-refresh))

     (defun dp-3-справа ()
       "Monitor on right with normal orientation."
       (start-process-shell-command
        "xrandr" nil "xrandr --output DP-3 --auto --rotate normal --output eDP-1 --auto --left-of DP-3 --primary")
       (exwm-randr-refresh))

     (defun dp-3-сверху ()
       "Monitor on top with normal orientation."
       (start-process-shell-command
        "xrandr" nil "xrandr --output DP-3 --auto --rotate normal --output eDP-1 --auto --below DP-3 --primary")
       (exwm-randr-refresh))

     ;; (defun monitor-on-right-rotate ()
     ;;   "monitor on top with normal orientation"
     ;;   (start-process-shell-command

     ;;    "xrandr" nil "xrandr --output VGA-1 --auto --rotate left --primary --output LVDS-1 --auto --left-of VGA-1"))

     (defun применить-расположение-монитора ()
       "Применить расположение монитора из переменной расположение-монитора"
       (let ((xrandr-position (cond
                              ((eq расположение-монитора 'сверху) "--below")
                              ((eq расположение-монитора 'справа) "--left-of")
                              (t "--left-of"))))
         (start-process-shell-command
          "xrandr" nil
          (concat "xrandr --output " имя-внешнего-монитора " --auto --rotate normal --output " имя-встроенного-монитора " --auto " xrandr-position " " имя-внешнего-монитора " --primary"))
         (exwm-randr-refresh)))

     (exwm-randr-enable)

     (add-hook 'exwm-randr-screen-change-hook
              'применить-расположение-монитора)))

(provide 'про-внешние-мониторы)
;;; внешние-мониторы.el ends here
