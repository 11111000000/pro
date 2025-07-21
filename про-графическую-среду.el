;;; про-графическую-среду.el --- Графическая среда Emacs с EXWM -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: exwm, window-manager, desktop, multi-monitor
;; URL: https://example.com/про-графическую-среду
;;
;;; Commentary:
;;
;; Этот файл настраивает графическую среду в Emacs на базе EXWM (Emacs X Window Manager),
;; следуя принципам литературного программирования: код представлен как coherentное повествование,
;; где каждая секция мотивируется, объясняется и логически связывается с остальными. Мы
;; стремимся к элегантности, минимализму и производительности в лучших традициях Emacs —
;; с использованием `use-package` для декларативной конфигурации, хуков для автоматизации
;; и отложенной загрузки для скорости.
;;
;; Почему это важно? EXWM превращает Emacs в полноценный оконный менеджер, интегрируя
;; X-приложения напрямую в буферы, что даёт унифицированный интерфейс для кода, документов
;; и внешних программ. Здесь мы фокусируемся на мультимониторной поддержке, клавиатурных
;; ярлыках, системном трее, автоматизации и утилитах (скриншоты, мягкое завершение),
;; с акцентом на стабильность и удобство.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и установки.
;; 1. Подсистема X: Xelb для низкоуровневого взаимодействия.
;; 2. EXWM: Основной оконный менеджер с настройками и клавишами.
;; 3. Инициализация: Поэтапный старт графической среды.
;; 4. Дополнительные возможности: Трей, редактор, фокус мыши.
;; 5. Утилиты: Скриншоты и очистка служебных буферов.
;; 6. Мягкое завершение: Закрытие окон и выключение системы.
;; 7. Блокировка случайных событий: Игнор ненужных комбо мыши.
;; 8. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-графическую-среду) в вашем init.el.
;; Рекомендуется интегрировать с про-мониторы для мультимониторов. Закомментированные
;; секции — опции для экспериментов (например, debug).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Запуск только в графическом режиме (window-system).

;;; Code:

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

;;;; 3. Инициализация
;; Поэтапный старт: мониторы, пауза, RandR, workspaces, клавиши, трей, хуки.
;; Мы гарантируем однократный запуск, чтобы избежать повторений, и автостарт в графическом режиме.

;; --- Стартовая инициализация EXWM. Выполняется только один раз!
(defvar про/графика-initialized nil
  "Истина, если графическая среда уже инициализирована.")

(defun про/старт-графической-среды ()
"Поэтапная инициализация графического окружения: мониторы, EXWM, трей, раскладка."
(interactive)
(unless про/графика-initialized
  (setq про/графика-initialized t)
  ;; -- 1. Физическое размещение мониторов (xrandr)
  (require 'про-мониторы)
  (применить-расположение-мониторов)
  ;; -- 2. Пауза, чтобы применились новые настройки дисплея
  (sleep-for про/monitor-refresh-delay)
  ;; -- 3. Включить RandR режим EMACS (до EXWM)
  (when (fboundp 'exwm-randr-mode)
    (exwm-randr-mode t))
  ;; -- 4. Инициализация сопоставлений workspace <-> monitor
  (про-мониторы-инициализировать)
  ;; -- 5. Запустить собственно EXWM
  (require 'exwm)
  ;; -- 6. Глобальные рабочие клавиши (Super+F1‒F9, Super+цифры)
  (dotimes (i 9)
    (exwm-input-set-key (kbd (format "s-<f%d>" i))
                        `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "S-s-<f%d>" i))
                        `(lambda () (interactive) (message ">%d" ,i)))
    (exwm-input-set-key (kbd (format "s-%d" i))
                        `(lambda () (interactive) (tab-bar-select-tab ,i))))
  (exwm-input-set-key (kbd "s-<f10>")
                      `(lambda () (interactive) (exwm-workspace-switch-create 0)))
  (setq exwm-input-simulation-keys exwm-input-simulation-keys)
  (push ?\C-\\ exwm-input-prefix-keys)  ;; Быстрая смена раскладки
  (exwm-wm-mode 1)
  ;; -- 7. Системный трей и XIM/импорт ввода
  (exwm-systemtray-mode t)
  (exwm-xim-mode t)
  ;; -- 8. Принудительная активация всех workspace, чтобы EXWM их закрепил за мониторами
  (dotimes (i exwm-workspace-number)
    (exwm-workspace-switch-create i))
  ;; -- 9. Хуки для красивых имён окон
  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer (concat exwm-class-name exwm-title))))
  (defun exwm-update-title-hook ()
    (exwm-workspace-rename-buffer (concat exwm-class-name ":" exwm-title)))
  (add-hook 'exwm-update-title-hook 'exwm-update-title-hook)
  ;; -- 10. Конфигурация специальных окон и поведение floating
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
          ))))

(use-package exwm
  :ensure t
  :if window-system
  :defer t
  :init
  (setq exwm-debug t
        exwm-workspace-number         3
        exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers   t
        exwm-manage-force-tiling      nil
        exwm-systemtray-background-color 'default
        exwm-systemtray-height         22)
  :config



  ;; -- Автоматический запуск, только если не консоль
  (when window-system
    (про/старт-графической-среды)))

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
  (let ((dir "~/Скриншоты/"))
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
  (let ((dir "~/Скриншоты/"))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (start-process-shell-command
     "scrot-full" nil
     (format "scrot '%s%%Y-%%m-%%d-%%H-%%M_$wx$h.png' -e 'copyq write image/png - < $f && copyq select 0'"
             dir))))

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
  (when (or force (yes-or-no-p "Выключить компьютер? "))
    (add-hook 'kill-emacs-hook
              (lambda ()
                (start-process "system-shutdown" nil "systemctl" "poweroff")))
    (save-buffers-kill-emacs)))

;; -- Универсальное сочетание для выключения (Super + Alt(M) + q)
(global-set-key (kbd "s-M-q") #'my/exwm-shutdown)

;;;; 7. Блокировка случайных Ctrl/Shift+мышь
;; Игнорируем комбо, чтобы избежать неожиданных меню в приложениях.
;; Это повышает стабильность, особенно в мультитач-средах.

(dolist (mod '([C-down-mouse-1] [C-mouse-1]
               [C-down-mouse-2] [C-mouse-2]
               [C-down-mouse-3] [C-mouse-3]
               [S-down-mouse-1] [S-mouse-1]
               [S-down-mouse-2] [S-mouse-2]
               [S-down-mouse-3] [S-mouse-3]))
  (global-set-key mod #'ignore))

;;;; 8. Финал
;; Завершаем модуль, предоставляя его для require в других файлах.

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
