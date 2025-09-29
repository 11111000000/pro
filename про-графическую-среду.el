;;; про-графическую-среду.el --- Графическая среда Emacs с EXWM -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: exwm, window-manager, desktop, multi-monitor
;; URL: ttps://example.com/про-графическую-среду
;;
;;; Commentary:
;;
;; Этот файл настраивает графическую среду в Emacs на базе EXWM (Emacs X Window Manager),
;; ...
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Запуск только в графическом режиме (window-system).

;;; Code:

(when (display-graphic-p)

;;;; 0. Введение и зависимости
  ;; Здесь мы подключаем базовые пакеты. `установить-из` — для установки из репозиториев,
  ;; `про-мониторы` — для мультимониторной поддержки. Это основа: без них EXWM не сможет
  ;; работать с X-сервером и мониторами.

  (require 'установить-из)
  (require 'про-мониторы)

;;;; 1. Подсистема X (xelb)
  ;; Xelb — низкоуровневая библиотека для взаимодействия с X11. Она необходима для EXWM,
  ;; предоставляя базовые протоколы. Мы загружаем её отложенно, только в графическом режиме.

  (use-package xelb
    :ensure t
    :if window-system)

;;;; 2. EXWM — главный оконный менеджер
  ;; EXWM превращает Emacs в WM, управляя X-окнами как буферами. Мы настраиваем
  ;; workspaces, буферы, tiling и клавиатурные эмуляции для Emacs-подобного поведения
  ;; в внешних приложениях. Это сердце файла: всё строится вокруг него.

  ;; --- Клавиатурные эмуляции стандартных перемещений/редактирования в X приложениях
  (defvar exwm-input-simulation-keys
    '(([?\C-b] . left)
      ([?\M-b] . C-left)
      ([?\C-f] . right)
      ([?\M-f] . C-right)
      ([?\C-p] . up)
      ([?\C-n] . down)
      ([?\C-a] . home)
      ([?\C-e] . end)
      ([?\M-v] . prior)
      ([?\C-v] . next)
      ([?\C-d] . ?\C-x)
      ([?\M-d] . (C-S-right delete))
      ;; cut/paste.
      ([?\M-y] . ?\C-c)
      ([?\M-w] . ?\C-c)
      ([?\C-y] . ?\C-v)
      ;; search
      ([?\C-s] . ?\C-f))
    "Сочетания клавиш, переотправляемых X приложениям для эмуляции behavior Emacs.")

  ;; --- Удобный макрос для глобальных горячих клавиш EXWM
  (defmacro exwm-input-set-keys (&rest key-bindings)
    "Установить горячие клавиши EXWM (глобально поверх X приложений).
KEY-BINDINGS — список пар (\"клавиша\" функция)."
    `(dolist (kb ',key-bindings)
       (cl-destructuring-bind (key cmd) kb
         (exwm-input-set-key (kbd key) cmd))))

  ;; --- Тачпад: отключать клики во время набора (Disable While Typing)
  (defun pro/disable-touchpad-while-typing-enable ()
    "Включить Disable-While-Typing для всех тачпадов libinput (X11).
Настраивает свойство 'libinput Disable While Typing Enabled' на 1 для всех найденных touchpad/trackpad/clickpad."
    (interactive)
    (let ((script
           "if command -v xinput >/dev/null 2>&1; then
  xinput list --name-only | grep -i -E 'touchpad|trackpad|clickpad' | while read dev; do
    if xinput list-props \"$dev\" 2>/dev/null | grep -q 'libinput Disable While Typing Enabled'; then
      xinput set-prop \"$dev\" 'libinput Disable While Typing Enabled' 1 || true
    fi
  done
fi"))
      (start-process-shell-command "dwt-touchpad" nil script)))

;;;; 3. Инициализация
  ;; Поэтапный старт: мониторы, пауза, RandR, workspaces, клавиши, трей, хуки.
  ;; Мы гарантируем однократный запуск, чтобы избежать повторений, и автостарт в графическом режиме.

  ;; --- Стартовая инициализация EXWM. Выполняется только один раз!
  (defvar pro/графика-initialized nil
    "Истина, если графическая среда уже инициализирована.")

  (defgroup pro/графика nil
    "Настройки графической среды EXWM."
    :group 'exwm)

  (defcustom pro/tray-restart-delay 6.0
    "Задержка (в секундах) перед рестартом exwm-session.target после запуска EXWM и трея.
Увеличьте, если иконки (например, nm-applet) не успевают зарегистрироваться."
    :type 'number
    :group 'pro/графика)

  (defun pro/старт-графической-среды ()
    "Поэтапная инициализация графического окружения: мониторы, EXWM, трей, раскладка."
    (interactive)
    (unless pro/графика-initialized
      (setq pro/графика-initialized t)
      ;; -- Физическое размещение мониторов (xrandr) ДО запуска EXWM
      (применить-расположение-мониторов)
      ;; -- Инициализация сопоставлений workspace <-> monitor
      (про-мониторы-инициализировать)
      ;; -- Запустить собственно EXWM
      (require 'exwm)
      ;; -- Глобальные рабочие клавиши (Super+F1‒F9, Super+цифры)
      (dotimes (i 9)
        (exwm-input-set-key (kbd (format "s-<f%d>" i))
                            `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
        (exwm-input-set-key (kbd (format "S-s-<f%d>" i))
                            `(lambda () (interactive) (message ">%d" ,i)))
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda () (interactive) (tab-bar-select-tab ,i))))
      (exwm-input-set-key (kbd "s-<f10>")
                          `(lambda () (interactive) (exwm-workspace-switch-create 0)))
      ;; Эмуляция «эмаксовских» клавиш в X-приложениях (для line-mode).
      ;; Текущая версия EXWM не содержит exwm-input-set-simulation-keys — задаём напрямую.
      (setq exwm-input-simulation-keys
            '(([?\C-b] . left)
              ([?\C-f] . right)
              ([?\C-p] . up)
              ([?\C-n] . down)
              ([?\C-a] . home)
              ([?\C-e] . end)
              ([?\M-v] . prior)
              ([?\C-v] . next)
              ([?\M-b] . C-left)
              ([?\M-f] . C-right)
              ;; Редактирование
              ([?\C-d] . delete)
              ([?\C-k] . (S-end delete))
              ;; Копирование/вставка (часто удобны в браузерах)
              ([?\M-w] . ?\C-c)
              ([?\C-y] . ?\C-v)))
      (push ?\C-\\ exwm-input-prefix-keys)  ;; Быстрая смена раскладки
      (exwm-wm-mode 1)
      ;; -- Браузеры: Firefox/Chromium — всегда line-mode + симуляция Emacs-навигирования
      (defun pro/exwm-браузеры-line+sim ()
        "Для Firefox/Chromium включать line-mode и локальные simulation-keys,
чтобы C-n/C-p и др. вели себя как в Emacs. Совместимо с exwm-xim-mode:
при необходимости можно вручную переключиться в char-mode (s-i)."
        (when (and (derived-mode-p 'exwm-mode)
                   exwm-class-name
                   (member (downcase exwm-class-name)
                           '("firefox" "chromium" "google-chrome" "chromium-browser")))
          (exwm-input-line-mode)
          (exwm-input-set-local-simulation-keys exwm-input-simulation-keys)))
      (add-hook 'exwm-manage-finish-hook #'pro/exwm-браузеры-line+sim)
      ;; -- RandR после запуска EXWM (иначе “not connected”)
      (when (fboundp 'exwm-randr-mode)
        (exwm-randr-mode 1)
        (ignore-errors (exwm-randr-refresh)))
      ;; -- Системный трей и XIM/импорт ввода
      (exwm-systemtray-mode t)
      (exwm-xim-mode t)
      ;; -- Отключать тачпад при наборе (Disable-While-Typing) — предотвращает случайные клики
      (when (display-graphic-p)
        (pro/disable-touchpad-while-typing-enable)
        ;; Повторно применять при изменении конфигурации дисплеев
        (with-eval-after-load 'exwm-randr
          (add-hook 'exwm-randr-screen-change-hook #'pro/disable-touchpad-while-typing-enable)))

      ;; -- Хуки для красивых имён окон
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (exwm-workspace-rename-buffer (concat exwm-class-name exwm-title))))
      (defun exwm-update-title-hook ()
        (exwm-workspace-rename-buffer (concat exwm-class-name ":" exwm-title)))
      (add-hook 'exwm-update-title-hook 'exwm-update-title-hook)
      ;; -- Конфигурация специальных окон и поведение floating
      (setq exwm-manage-configurations
            `(
              ;; Blueman Applet/Manager — не floating и управляются
              ((or (string= exwm-class-name "Blueman-manager")
                   (string= exwm-class-name "Blueman-applet")
                   (and exwm-title (string-match "blueman" exwm-title)))
               floating nil
               managed t)
              ;; posframe — floating окно без mode line
              ((equal exwm-title "posframe") floating t floating-mode-line nil)
              ;; chromebug — спец. размеры и fixed позиция
              ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
               height 175 x 30 y 30 managed t)
              ))
      ;; -- Копипаста
      (require 'xclip)
      (xclip-mode 1)

      ;; перезапускаем user graphical-session.target, чтобы треевые сервисы стартовали уже при наличии хоста трея (EXWM).
      ;; Важно: здесь необходима небольшая задержка (например, 1 секунда), чтобы EXWM успел полностью инициализировать трей.
      ;; Иначе сервис exwm-session.target может запуститься ДО появления трея и не интегрироваться с ним (race condition)!
      (run-at-time pro/tray-restart-delay nil ;; асинхронная задержка перед рестартом exwm-session.target
                   (lambda ()
                     (ignore-errors
                       (start-process "systemctl-user" nil "systemctl" "--user" "restart" "exwm-session.target"))))
      )))

(use-package xclip
  :ensure t)

(use-package exwm
  :ensure t
  :if window-system
  :defer t
  :init
  (setq exwm-debug nil
        exwm-workspace-number         3
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers   t
        exwm-manage-force-tiling      nil
        exwm-systemtray-background-color 'default
        exwm-systemtray-height         22
        ;; Точные пиксельные ресайзы, минимизация промежуточных пересчётов
        frame-resize-pixelwise t
        window-resize-pixelwise t
        frame-inhibit-implied-resize t)
  :config
  ;; Без этого Emacs остаётся жить после kill X, держит сокет :0,
  ;; SDDM не может поднять новый сервер и уходит в restart-loop.
  (add-hook 'exwm-exit-hook #'save-buffers-kill-emacs)


  ;; -- Автоматический запуск, только если не консоль
  (when window-system
    (pro/старт-графической-среды)))

;;;; 4. Дополнительные возможности EXWM
;; Расширения для удобства: трей, редактор текстовых полей, фокус мыши.
;; Они дополняют базовый EXWM, делая его полноценным десктопом.

;; -- Системный трей (иконки в панели)
(require 'exwm-systemtray)

;; -- Встроенный редактор для внешних текстовых полей
(use-package exwm-edit
  :after exwm
  :if window-system
  :ensure t)

;; -- Курсор мыши автоматически следует за фокусом окна
(use-package exwm-mff
  :after exwm
  :if window-system
  :functions (exwm-mff-mode)
  :ensure t
  :init
  (exwm-mff-mode t))

;;;; 5. Утилиты: Скриншоты и очистка
;; Практичные инструменты: скриншоты в clipboard, очистка "мёртвых" буферов.
;; Это упрощает повседневные задачи, интегрируя с copyq и мониторя конфигурацию окон.

(defun скриншот-области ()
  "Снять скриншот выделенной области и отправить в буфер обмена (copyq).
Сохраняет файлы в ~/Скриншоты/"
  (interactive)
  (let* ((dir (expand-file-name "~/Скриншоты/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (start-process-shell-command
     "scrot-area" nil
     (format "scrot -s '%s%%Y-%%m-%%d_%%H.%%M.%%S.png' -e 'copyq write image/png - < $f && copyq select 0'"
             dir))))

(defun скриншот ()
  "Сделать скриншот всего экрана и отправить в copyq.
Сохраняет файлы в ~/Скриншоты/"
  (interactive)
  (sit-for 1)
  (let* ((dir (expand-file-name "~/Скриншоты/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (start-process-shell-command
     "scrot-full" nil
     (format "scrot -d 3 '%s%%Y-%%m-%%d-%%H-%%M_$wx$h.png' -e 'copyq write image/png - < $f && copyq select 0'"
             dir))))

(defun скринкаст (&optional duration)
  "Запустить скринкаст экрана с помощью ffmpeg.
Файл сохраняется в ~/Скринкасты/. Лог пишется в отдельный буфер, его можно просмотреть командой `скринкаст-показать-лог`.
Если DURATION задан (в секундах), запись завершится автоматически."
  (interactive "P")
  (let* ((dir (expand-file-name "~/Скринкасты/"))
         (_ (unless (file-directory-p dir)
              (make-directory dir t)))
         (fname (read-string "Имя скринкаста (без расширения): "
                             (format-time-string "%Y-%m-%d_%H-%M-%S")))
         (path (expand-file-name (concat fname ".mkv") dir))
         (log-buf (get-buffer-create (format "*скринкаст %s*" fname)))
         (size (string-trim
                (shell-command-to-string
                 "xrandr | grep '*' | head -n1 | awk '{print $1}'")))
         (display (or (getenv "DISPLAY") ":0"))
         (args (append
                (list
                 "-video_size" size
                 "-framerate" "30"
                 "-f" "x11grab"
                 "-i" display
                 "-c:v" "libx264rgb"
                 "-crf" "0"
                 "-preset" "ultrafast")
                (when duration (list "-t" (format "%s" duration)))
                (list path)))
         (proc nil))
    (with-current-buffer log-buf (erase-buffer))
    (setq proc (apply #'start-process
                      "ffmpeg-с-кринкаст" log-buf "ffmpeg" args))
    (setq скринкаст-процесс proc)
    (setq скринкаст-лог-був log-buf)
    (message "Скринкаст запущен: %s (лог в %s, остановка: M-x скринкаст-стоп)"
             path (buffer-name log-buf))))

(defun скринкаст-показать-лог ()
  "Показать лог последнего процесса скринкаста (буфер ffmpeg)."
  (interactive)
  (if (and (boundp 'скринкаст-лог-був) (buffer-live-p скринкаст-лог-був))
      (pop-to-buffer скринкаст-лог-був)
    (message "Лог ещё не создан")))

(defun скринкаст-стоп ()
  "Остановить запись скринкаста (убить процесс ffmpeg для скринкаста)."
  (interactive)
  (if (and (boundp 'скринкаст-процесс) (process-live-p скринкаст-процесс))
      (progn (kill-process скринкаст-процесс)
             (message "Скринкаст остановлен (процесс ffmpeg завершён)"))
    (message "Нет активного процесса скринкаста! Используйте killall ffmpeg для остановки всех ffmpeg.")))

;; -- Очистка служебных буферов EXWM ("dead window")
(defun my/kill-old-buffer-message-buffer-maybe ()
  "Если буфер содержит служебное сообщение EXWM о dead window — закрыть его."
  (when (and (< (buffer-size) 2000)
             (string-match-p "^This window displayed the buffer ‘"
                             (buffer-string)))
    (let ((buf (current-buffer)))
      (when (get-buffer-window buf)
        (delete-window (get-buffer-window buf)))
      (kill-buffer buf))))

(defun my/kill-old-buffer-message-buffers-in-visible-windows ()
  "Проверить все окна, закрыть буфер, если это служебное сообщение о dead window."
  (dolist (win (window-list))
    (let ((buf (window-buffer win)))
      (when (and
             (< (buffer-size buf) 2000)
             (with-current-buffer buf
               (string-match-p "^This window displayed the buffer ‘" (buffer-string))))
        (ignore-errors (delete-window win))
        (kill-buffer buf)))))

;; -- Удалять служебные буферы при каждом изменении конфигурации окон
(add-hook 'window-configuration-change-hook
          #'my/kill-old-buffer-message-buffers-in-visible-windows)

;;;; 6. Мягкое завершение X окружения/Emacs/компьютера
;; Грациозный шатдаун: закрытие окон, Emacs и системы "в одну кнопку".
;; Это предотвращает потери данных и упрощает выход.

(defun my/exwm-close-all-windows ()
  "Закрыть все внешние X-приложения, управляемые EXWM."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (eq major-mode 'exwm-mode)
        ;; Каждое kill-buffer вызовет WM_DELETE_WINDOW/close через EXWM
        (kill-buffer buf)))))

(defun my/exwm-running-x-buffers-p ()
  "Есть ли ещё активные EXWM-буферы (живые X-клиенты)?"
  (seq-some (lambda (buf)
              (with-current-buffer buf
                (eq major-mode 'exwm-mode)))
            (buffer-list)))

(defun my/exwm-shutdown (&optional force)
  "Мягко завершить все X-приложения, потом Emacs, затем выключить компьютер.
Если FORCE — не спрашивать подтверждения."
  (interactive)
  (my/exwm-close-all-windows)
  (dotimes (_ 10) ;; 10 попыток с паузой
    (when (my/exwm-running-x-buffers-p)
      (sleep-for 1)
      (my/exwm-close-all-windows)))
  (save-buffers-kill-emacs))

;; -- Универсальное сочетание для выключения (Super + Alt(M) + q)
(global-set-key (kbd "s-M-q") #'my/exwm-shutdown)

;;;; 6.9 EXWM: Логи для диагностики симуляции клавиш (C-n/C-p в браузерах)

(defvar pro/exwm-keys-log-buffer "*EXWM-keys-log*"
  "Буфер, куда пишем диагностические сообщения о симуляции клавиш EXWM.")

(defun pro/exwm-log (&rest args)
  "Записать строку в лог и вывести в echo-area."
  (let* ((msg (apply #'format args))
         (ts  (format-time-string "%F %T.%3N")))
    (with-current-buffer (get-buffer-create pro/exwm-keys-log-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s\n" ts msg)))
    (message "%s" msg)))

(defun pro/exwm-show-keys-log ()
  "Открыть буфер с логами EXWM-симуляции клавиш."
  (interactive)
  (pop-to-buffer (get-buffer-create pro/exwm-keys-log-buffer)))

(defvar-local pro/exwm--mode 'unknown
  "Текущий режим ввода для EXWM-буфера: 'line, 'char или 'unknown.")

(with-eval-after-load 'exwm
  ;; Отмечаем смену режимов (line/char) и логируем
  (advice-add 'exwm-input-line-mode :after
              (lambda (&rest _)
                (setq pro/exwm--mode 'line)
                (pro/exwm-log "→ line-mode для %s (%s)"
                              (or (bound-and-true-p exwm-class-name) "?")
                              (buffer-name))))
  (advice-add 'exwm-input-char-mode :after
              (lambda (&rest _)
                (setq pro/exwm--mode 'char)
                (pro/exwm-log "→ char-mode для %s (%s)"
                              (or (bound-and-true-p exwm-class-name) "?")
                              (buffer-name))))
  ;; Логируем фактическую отправку fake-keys во внешний клиент
  (advice-add 'exwm-input--fake-key :around
              (lambda (orig key &rest args)
                (pro/exwm-log "fake-key: %S -> class=%s buf=%s mode=%s"
                              key
                              (or (bound-and-true-p exwm-class-name) "?")
                              (buffer-name)
                              pro/exwm--mode)
                (apply orig key args))))

(defun pro/exwm--sim-mapping (key)
  "Вернуть cons-мэппинг из (local/global) simulation-keys для KEY."
  (or (and (boundp 'exwm-local-simulation-keys)
           (assoc key exwm-local-simulation-keys))
      (and (boundp 'exwm-input-simulation-keys)
           (assoc key exwm-input-simulation-keys))))

(defun pro/exwm--log-state (trigger)
  "Снять срез состояния по TRIGGER (строка «C-n»/«C-p» и т.д.)."
  (pro/exwm-log "%s: class=%s buf=%s mode=%s xim=%s local-sim=%S"
                trigger
                (or (and (boundp 'exwm-class-name) exwm-class-name) "?")
                (buffer-name)
                pro/exwm--mode
                (if (bound-and-true-p exwm-xim-mode) 'on 'off)
                (and (boundp 'exwm-local-simulation-keys)
                     exwm-local-simulation-keys)))

(defun pro/exwm-C-n ()
  "Логирующий обработчик C-n для EXWM-буфера браузера."
  (interactive)
  (pro/exwm--log-state "C-n")
  (let ((m (pro/exwm--sim-mapping [?\C-n])))
    (pro/exwm-log "C-n: mapping=%S; вызываю exwm-input-send-simulation-keys" m)
    ;; Пытаемся использовать штатную симуляцию; если маппинга нет,
    ;; EXWM обычно пошлёт исходный ключ клиенту — это тоже видно по эффекту.
    (when (fboundp 'exwm-input-send-simulation-keys)
      (exwm-input-send-simulation-keys (kbd "C-n")))))

(defun pro/exwm-C-p ()
  "Логирующий обработчик C-p для EXWM-буфера браузера."
  (interactive)
  (pro/exwm--log-state "C-p")
  (let ((m (pro/exwm--sim-mapping [?\C-p])))
    (pro/exwm-log "C-p: mapping=%S; вызываю exwm-input-send-simulation-keys" m)
    (when (fboundp 'exwm-input-send-simulation-keys)
      (exwm-input-send-simulation-keys (kbd "C-p")))))

(defun pro/exwm-браузеры-line+sim ()
  "Для Firefox/Chromium: включить line-mode и локальные simulation-keys."
  (when (and (derived-mode-p 'exwm-mode)
             (boundp 'exwm-class-name)
             exwm-class-name
             (member (downcase exwm-class-name)
                     '("firefox" "chromium" "google-chrome" "chromium-browser")))
    (pro/exwm-log "exwm-manage: %s → line-mode + локальные simulation-keys (xim=%s)"
                  exwm-class-name (if (bound-and-true-p exwm-xim-mode) 'on 'off))
    (when (fboundp 'exwm-input-line-mode)
      (exwm-input-line-mode))
    (setq-local pro/exwm--mode 'line)
    (when (fboundp 'exwm-input-set-local-simulation-keys)
      (exwm-input-set-local-simulation-keys exwm-input-simulation-keys)
      (pro/exwm-log "local-sim set: %S"
                    (and (boundp 'exwm-local-simulation-keys)
                         exwm-local-simulation-keys)))))

(with-eval-after-load 'exwm
  (add-hook 'exwm-manage-finish-hook #'pro/exwm-браузеры-line+sim))

;;;; 7. Блокировка случайных Ctrl/Shift+мышь
;; Игнорируем комбо, чтобы избежать неожиданных меню в приложениях.

(dolist (mod '([C-down-mouse-1] [C-mouse-1]
               [C-down-mouse-2] [C-mouse-2]
               [C-down-mouse-3] [C-mouse-3]
               [S-down-mouse-1] [S-mouse-1]
               [S-down-mouse-2] [S-mouse-2]
               [S-down-mouse-3] [S-mouse-3]))
  (global-set-key mod #'ignore))

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
