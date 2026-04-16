;;; про-графическую-среду.el --- Графическая среда и EXWM -*- lexical-binding: t -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: exwm, window-manager, desktop, multi-monitor
;; URL: https://github.com/username/emacs.d/blob/main/среда/про-графическую-среду.el
;;
;;; Commentary:
;;
;; Этот файл настраивает графическую среду Emacs на базе EXWM (Emacs X Window Manager).
;; Фокус здесь на запуске WM, мульти-мониторной инициализации, системном трее,
;; XIM и именовании EXWM-окон. Вспомогательные утилиты вынесены в отдельный модуль
;; `про-графические-утилиты`, чтобы этот файл оставался про среду, а не про
;; общие команды.
;;
;; Структура файла:
;; 0. Введение и зависимости: базовые require и пакетная рамка.
;; 1. Базовые данные: simulation keys и классы окон.
;; 2. Общие вспомогатели: трей, RandR, touchpad и имена окон.
;; 3. Инициализация EXWM: запуск, хуки и конфигурация поведения.
;; 4. Дополнительные возможности: EXWM-расширения, подключаемые модули.
;; 5. Утилиты: вынесены в `про-графические-утилиты`.
;;
;; Файл рассчитан на загрузку только в графической сессии.

;;; Code:

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "про-графическую-среду load entered"))

;;;; 0. Введение и зависимости
;; Здесь подключаем базовые пакеты и модули, которые нужны до старта EXWM.
;; `установить-из` отвечает за установку зависимостей, `про-мониторы` — за
;; вычисление и применение физической раскладки дисплеев.

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "require установить-из begin"))
(require 'установить-из)
(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "require установить-из done"))

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "require про-мониторы begin"))
(require 'про-мониторы)
(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "require про-мониторы done"))

(unless (featurep 'установить-из)
  (defun установить-из (&rest _args)
    "Fallback no-op during partial loads."
    nil))

;;;; 1. Подсистема X (xelb)
;; Xelb дает низкоуровневый доступ к X11 и нужен EXWM как транспортный слой.
;; Загружаем его только в графической сессии.

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "use-package xelb begin"))
(use-package xelb
  :ensure t
  :if window-system)
(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "use-package xelb done"))

;;;; 2. Базовые данные EXWM
;; Здесь лежат неизменяемые списки и макросы, которые используются на этапе старта.
;; Они отделены от инициализации, чтобы логика запуска оставалась читаемой.

;; --- Клавиатурные эмуляции стандартных перемещений/редактирования в X приложениях
(defconst pro/exwm-default-simulation-keys
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

(defconst pro/exwm-browser-classes
  '("firefox" "chromium" "google-chrome" "chromium-browser"
    "brave-browser" "vivaldi-stable")
  "Классы окон, которые надо считать браузерами.")

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
;; Здесь собирается поэтапный старт: мониторы, EXWM, трей, XIM, хуки.
;; Функция `pro/старт-графической-среды` остается тонкой точкой сборки шагов.

;; --- Стартовая инициализация EXWM. Выполняется только один раз!
(defvar pro/графика-initialized nil
  "Истина, если графическая среда уже инициализирована.")

(defvar pro/exwm-booted nil
  "Истина, если EXWM реально поднялся и exwm-init-hook отработал.")

;; Пользовательские параметры графической среды.
(defgroup pro/графика nil
  "Настройки графической среды EXWM."
  :group 'exwm)

;; Передает X11-окружение в systemd --user, чтобы tray-сервисы видели EXWM.
(defun pro/exwm-import-display-env ()
  "Передать DISPLAY/XAUTHORITY в systemd --user."
  (let ((display (or (getenv "DISPLAY") ":0"))
        (xauth (or (getenv "XAUTHORITY")
                   (expand-file-name "~/.Xauthority"))))
    (setenv "DISPLAY" display)
    (setenv "XAUTHORITY" xauth)
    (ignore-errors
      (start-process "systemctl-user-import" nil
                     "systemctl" "--user" "import-environment"
                     "DISPLAY" "XAUTHORITY"))))

;; Готовит окружение для EXWM system tray.
(defun pro/exwm-configure-systemtray ()
  "Подготовить окружение для system tray."
  (pro/exwm-import-display-env))

;; Отслеживает ВСЕ вызовы exwm-systemtray-mode (включение и выключение).
;; Добавлено для диагностики — показывает, не отключается ли трей где-то после включения.
(defun pro/exwm-systemtray-mode-advice (orig-fn &rest args)
  "Перехватывает вызовы exwm-systemtray-mode для диагностики."
  (message "PRO: exwm-systemtray-mode вызван: args=%s" args)
  (let ((val (apply orig-fn args)))
    (message "PRO: exwm-systemtray-mode=%s после вызова" exwm-systemtray-mode)
    val))

(advice-add 'exwm-systemtray-mode :around #'pro/exwm-systemtray-mode-advice)

(defvar pro/exwm-systemtray-log-timer nil
  "Timer for periodic system tray diagnostics.")

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "core definitions done"))

(defun pro/exwm-log-systemtray-state (&optional stage)
  "Log the current EXWM system tray state."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "systemtray" (format "begin %s" (or stage "?"))))
  (let ((target (string-trim
                 (shell-command-to-string
                  "systemctl --user is-active exwm-session.target 2>/dev/null || true"))))
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "systemtray" (format "after systemctl %s" (or stage "?"))))
    (message "PRO: systemtray[%s] feature=%s mode=%s booted=%s target-active=%s"
             (or stage "?")
             (featurep 'exwm-systemtray)
             (and (boundp 'exwm-systemtray-mode) exwm-systemtray-mode)
             (and (boundp 'pro/exwm-booted) pro/exwm-booted)
             (if (string-empty-p target) "unknown" target))))

;; Включает EXWM system tray после инициализации EXWM.
(defun pro/exwm-enable-systemtray ()
  "Включить системный трей и запустить user target для EXWM-сессии."
  (message "PRO: exwm-enable-systemtray вызвана")
  (pro/exwm-log-systemtray-state "enable-before")
  (require 'exwm-systemtray nil t)
  (when (fboundp 'exwm-systemtray-mode)
    (message "PRO: exwm-systemtray-mode ДО=%s" exwm-systemtray-mode)
    (exwm-systemtray-mode t)
    (message "PRO: exwm-systemtray-mode ПОСЛЕ=%s" exwm-systemtray-mode))
  (pro/exwm-log-systemtray-state "enable-after")
  (run-at-time 1 nil
               (lambda ()
                 (pro/exwm-log-systemtray-state "enable-delayed")
                 (ignore-errors
                   (start-process "systemctl-user-reset-failed" nil
                                  "systemctl" "--user" "reset-failed")
                   (start-process "systemctl-user-start" nil
                                  "systemctl" "--user" "start" "exwm-session.target")))))

(defun pro/exwm-wm-mode-safe ()
  "Запустить `exwm-wm-mode` с диагностикой."
  (message "PRO: вызываю exwm-wm-mode 1 (featurep exwm=%s, fn=%s)"
           (featurep 'exwm) (fboundp 'exwm-wm-mode))
  (exwm-wm-mode 1)
  (message "PRO: exwm-wm-mode 1 выполнен успешно"))

;; Подключает XIM и стандартные simulation keys для внешних приложений.
(defun pro/exwm-configure-input-methods ()
  "Включить XIM и стандартные simulation keys."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "begin configure-input-methods"))
  (require 'exwm-xim nil t)
  (when (fboundp 'exwm-xim-mode)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "input-methods" "before exwm-xim-mode"))
    (exwm-xim-mode t))
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "before set simulation keys"))
  (setq exwm-input-simulation-keys pro/exwm-default-simulation-keys)
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "after set simulation keys"))
  (unless (member ?\C-\\ exwm-input-prefix-keys)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "input-methods" "before push prefix key"))
    (push ?\C-\\ exwm-input-prefix-keys))
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "end configure-input-methods")))

;; Активирует RandR и обновляет раскладку экранов после старта EXWM.
(defun pro/exwm-configure-randr ()
  "Включить RandR и обновить конфигурацию экранов."
  (when (fboundp 'exwm-randr-mode)
    (exwm-randr-mode 1)
    (ignore-errors (exwm-randr-refresh))))

;; Включает защиту от случайных кликов тачпадом во время набора.
(defun pro/exwm-configure-touchpad ()
  "Включить защиту от случайных кликов тачпадом."
  (when (display-graphic-p)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "touchpad" "begin configure-touchpad"))
    (pro/disable-touchpad-while-typing-enable)
    (with-eval-after-load 'exwm-randr
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "touchpad" "install randr hook"))
      (add-hook 'exwm-randr-screen-change-hook
                #'pro/disable-touchpad-while-typing-enable))
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "touchpad" "end configure-touchpad"))))

;; Детектирует браузеры для правил именования окна.
(defun pro/exwm-browser-p ()
  "Ненулевой результат, если текущее окно похоже на браузер."
  (let ((class (and (boundp 'exwm-class-name) exwm-class-name)))
    (and class
         (member (downcase class) pro/exwm-browser-classes))))

;; Возвращает title окна EXWM, если он уже доступен.
(defun pro/exwm-window-title ()
  "Вернуть заголовок окна EXWM, если он есть."
  (when (and (boundp 'exwm-title) exwm-title (> (length exwm-title) 0))
    exwm-title))

;; Возвращает WM_CLASS окна EXWM, если он уже доступен.
(defun pro/exwm-window-class ()
  "Вернуть класс окна EXWM, если он есть."
  (when (and (boundp 'exwm-class-name) exwm-class-name (> (length exwm-class-name) 0))
    exwm-class-name))

;; Правило именования: сначала title для браузеров, затем class, instance и fallback.
(defun pro/exwm-app-name ()
  "Вернуть имя для EXWM-буфера: title, class, instance или EXWM."
  (or (pro/exwm-window-title)
      (pro/exwm-window-class)
      (and (boundp 'exwm-instance-name) exwm-instance-name)
      "EXWM"))

;; Переименовывает активный EXWM-буфер по текущим данным окна.
(defun pro/exwm-rename-buffer ()
  "Переименовать текущий EXWM-буфер."
  (when (derived-mode-p 'exwm-mode)
    (let ((name (pro/exwm-app-name)))
      (when (and name (> (length name) 0))
        (exwm-workspace-rename-buffer name)))))

;; Применяет правила именования ко всем уже открытым EXWM-буферам.
(defun pro/exwm-rename-all-buffers ()
  "Переименовать все текущие EXWM-буферы по тем же правилам."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'exwm-mode)
        (pro/exwm-rename-buffer)))))

;; Регистрирует хуки, чтобы новые окна и обновления title/class переименовывались автоматически.
(defun pro/exwm-setup-window-hooks ()
  "Настроить хуки переименования EXWM-окон."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "hooks" "begin setup-window-hooks"))
  (add-hook 'exwm-update-class-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-manage-finish-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-init-hook #'pro/exwm-rename-all-buffers)
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "hooks" "end setup-window-hooks")))

;; Главная точка старта графической среды.
(defun pro/старт-графической-среды ()
  "Поэтапная инициализация графического окружения: мониторы, EXWM, трей, раскладка."
  (interactive)
  (message "PRO: про/старт-графической-среды вызвана")
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "exwm-start" "pro/старт-графической-среды entered"))
  (unless pro/графика-initialized
    (setq pro/графика-initialized t)
    (catch 'pro/exwm-start
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "applying monitor layout"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before monitor layout"))
      ;; Сначала применяем физическую раскладку дисплеев до запуска EXWM.
      (применить-расположение-мониторов)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "initializing monitor mapping"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before monitor mapping"))
      ;; Затем строим соответствие workspace <-> monitor.
      (про-мониторы-инициализировать)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "requiring exwm"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before require exwm"))
      ;; Загружаем EXWM только после подготовки дисплеев.
      (require 'exwm)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "after require exwm"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after require exwm"))
      ;; Глобальные рабочие клавиши для переключения workspace и tab-bar.
      (dotimes (i 9)
        (exwm-input-set-key (kbd (format "s-<f%d>" i))
                            `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
        (exwm-input-set-key (kbd (format "S-s-<f%d>" i))
                            `(lambda () (interactive) (message ">%d" ,i)))
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda () (interactive) (tab-bar-select-tab ,i))))
      (exwm-input-set-key (kbd "s-<f10>")
                          `(lambda () (interactive) (exwm-workspace-switch-create 0)))
      (condition-case err
          (pro/exwm-wm-mode-safe)
        (error
         (setq pro/графика-initialized nil)
         (when (fboundp 'pro/log-startup-stage)
           (pro/log-startup-stage "exwm-error" (format "%S" err)))
         (when (fboundp 'pro/startup-log)
           (pro/startup-log "module" (format "exwm-wm-mode error %S" err)))
         (message "PRO: exwm-wm-mode ОШИБКА: %S" err)
         (throw 'pro/exwm-start nil)))
      (message "PRO: exwm-wm-mode запущен, жду exwm-init-hook; pro/exwm-booted=%s" pro/exwm-booted)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after exwm-wm-mode"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "systemtray" "calling post-exwm-wm-mode log"))
      (pro/exwm-log-systemtray-state "post-exwm-wm-mode")
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "systemtray" "returned post-exwm-wm-mode log"))
      ;; RandR включаем после старта EXWM, иначе некоторые конфигурации не находятся.
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before configure-randr"))
      (pro/exwm-configure-randr)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after configure-randr"))
      ;; Системный трей включаем уже на живом XEmbed host.
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before configure-systemtray"))
      (pro/exwm-configure-systemtray)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after configure-systemtray"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before configure-input-methods"))
      (pro/exwm-configure-input-methods)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after configure-input-methods"))
      ;; Защита тачпада от случайных кликов при наборе.
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before configure-touchpad"))
      (pro/exwm-configure-touchpad)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after configure-touchpad"))
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "before setup-window-hooks"))
      (pro/exwm-setup-window-hooks)
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "module" "after setup-window-hooks"))
      ;; Правила для специальных окон и floating-поведения.

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

      ;; Интеграция с xclip для буфера обмена.
      (when (require 'xclip nil t)
        (when (fboundp 'xclip-mode)
          (xclip-mode 1)))

      ;; Флаг нужен, чтобы не завершать обычный Emacs, если EXWM так и не поднялся.
      (setq pro/exwm-booted nil)
      (add-hook 'exwm-init-hook
                (lambda ()
                  (message "PRO: exwm-init-hook вызван!")
                  (when (fboundp 'pro/log-startup-stage)
                    (pro/log-startup-stage "exwm-init-hook" "fired"))
                  (when (fboundp 'pro/startup-log)
                    (pro/startup-log "module" "exwm-init-hook fired"))
                  (setq pro/exwm-booted t)
                  (pro/exwm-import-display-env)
                  (pro/exwm-log-systemtray-state "init-hook-before")
                  (pro/exwm-enable-systemtray)
                  (pro/exwm-log-systemtray-state "init-hook-after")
                  (pro/exwm-rename-all-buffers))))))

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "про-графическую-среду load finished"))

;;;; 4. Дополнительные возможности EXWM
;; Расширения EXWM подключаем отдельно, чтобы базовый запуск оставался компактным.

(use-package xclip
  :ensure t)

(use-package exwm
  :ensure t
  :if window-system
  :defer t
  :init
  (setq exwm-debug nil
        exwm-workspace-number 3
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t
        exwm-manage-force-tiling nil
        exwm-systemtray-background-color 'default
        ;; Точные пиксельные ресайзы, минимизация промежуточных пересчётов
        frame-resize-pixelwise t
        window-resize-pixelwise t
        frame-inhibit-implied-resize t)
  :config
  ;; Без этого Emacs остаётся жить после kill X, держит сокет :0,
  ;; SDDM не может поднять новый сервер и уходит в restart-loop.
  ;; Но убиваем Emacs только если EXWM реально успел стартовать.
  (add-hook 'exwm-exit-hook
            (lambda ()
              (when (bound-and-true-p pro/exwm-booted)
                (save-buffers-kill-emacs))))


  ;; -- Автоматический запуск, только если не консоль
  (when window-system
    (pro/старт-графической-среды)))

;; Системный трей (иконки в панели) предоставляется отдельным модулем EXWM.
(require 'exwm-systemtray nil t)

;; Вспомогательные графические команды: скриншоты, скринкасты и shutdown.
(require 'про-графические-утилиты)

;; Встроенный редактор для внешних текстовых полей.
(use-package exwm-edit
  :after exwm
  :if window-system
  :ensure t)

;; Курсор мыши автоматически следует за фокусом окна.
(use-package exwm-mff
  :after exwm
  :if window-system
  :functions (exwm-mff-mode)
  :ensure t
  :init
  (when (fboundp 'exwm-mff-mode)
    (exwm-mff-mode t)))

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
