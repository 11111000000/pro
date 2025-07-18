;;; про-графическую-среду.el --- Оконный менеджер ExWM со всем окружением -*- lexical-binding: t -*-
;;; Commentary:

;; Настройка Emacs X Window Manager: мультимониторная работа, ярлыки, системный трей,
;; автоматизация EXWM, мягкое завершение работы и дополнительные функции (скриншоты и пр).
;;
;; Опорные зависимости: xelb, exwm, exwm-edit, exwm-mff
;;
;; Поддержка русского языка, комментарии подробно поясняют назначение блоков.

;;; Code:

;;;; Зависимости

(require 'установить-из)
(require 'про-мониторы)

;;;; Подсистема X (xelb)

(use-package xelb
  :ensure t
  :if window-system)

;;;; EXWM — главный оконный менеджер

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
              )))
    )

  ;; -- Автоматический запуск, только если не консоль
  (when window-system
    (про/старт-графической-среды)))

;;;; Дополнительные возможности EXWM

;; -- Системный трей (иконки в панели)
(require 'exwm-systemtray)

;; -- Встроенный редактор для внешних текстовых полей
(use-package exwm-edit
  :after exwm
  :if window-system
  :ensure t)

;; -- Курсоp мыши автоматически следует за фокусом окна
(use-package exwm-mff
  :after exwm
  :if window-system
  :functions (exwm-mff-mode)
  :ensure t
  :init
  (exwm-mff-mode t))

;;;; Утилиты: Скриншоты

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

;;;; Очистка служебных буферов EXWM ("dead window")

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

;;;; Мягкое завершение X окружения/Emacs/компьютера "в одну кнопку"

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

;;;; Блокировка случайных Ctrl/Shift+мышь (чтобы не было неожиданных меню)

(dolist (mod '([C-down-mouse-1] [C-mouse-1]
               [C-down-mouse-2] [C-mouse-2]
               [C-down-mouse-3] [C-mouse-3]
               [S-down-mouse-1] [S-mouse-1]
               [S-down-mouse-2] [S-mouse-2]
               [S-down-mouse-3] [S-mouse-3]))
  (global-set-key mod #'ignore))

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
