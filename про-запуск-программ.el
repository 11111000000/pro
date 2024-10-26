(let ((keys-table '(("pgrep nm-applet && killall -9 nm-applet" "Убираем запущенный апплет") ("dbus-launch nm-applet" "Сетевой апплет") ("dbus-launch blueman-applet" "Блутус апплет") ("dbus-launch udiskie -t" "Управление дисками") ("dbus-launch dunst -conf ~/System/dunstrc" "Нотификации") ("dbus-launch pasystray" "Микшер в трее") ("copyq" "Буфер обмена"))))
(require 'cl-lib)
(let ((commands (mapcar 'car keys-table))  ;; Извлечение команд
     (programs (mapcar 'cadr keys-table))  ;; Извлечение примечаний
     (delay 0.1)) ;; Начальная задержка
  (cl-loop for (cmd program) in (cl-mapcar 'list commands programs)
       do
       (run-at-time delay nil
                    (lambda (command prog)
                      (message "Запуск: %s [%s]" prog command)  ;; Печать сообщения с именем программы
                      (async-shell-command command))
                    cmd program)
       (setq delay (+ delay 0.1))))


)
