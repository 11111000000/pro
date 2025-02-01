;;; про-внешний-вид.el --- Внешний вид и Интерфейс  -*- lexical-binding: t -*-
;; Этот файл настраивает внешний вид и интерфейс Emacs.
;;; Commentary:
;;; Здесь описываются различные настройки для улучшения интерфейса и внешнего вида в Emacs.
;;; Code:

(require 'use-package)
(require 'установить-из)

;;;; Общий вид

;; Отключение звуковых оповещений и миганий для более спокойной работы.

(setq visible-bell nil)                            ; Отключение видимого звоночка.
(setq ring-bell-function 'ignore)                  ; Игнорирование звукового сигнала.

;; Скрытие панели инструментов и меню для упрощения интерфейса в графических средах.

(if window-system
    (tool-bar-mode -1))                            ; Скрыть панель инструментов.

(if window-system
    (menu-bar-mode -1))                            ; Скрыть меню.

;; Скрытие полосы прокрутки для более чистого интерфейса.

(if window-system (scroll-bar-mode -1))          ; Скрыть полосы прокрутки в графическом интерфейсе.

;;;; Минибуфер

;; Настройки для улучшения работы с минибуфером.

(setq-default enable-recursive-minibuffers t)     ;  рекурсивные минибуферы
(setq-default max-mini-window-height 0.25)          ; Минибуфер может быть любого размера.
(setq resize-mini-windows t)                       ; Автоматический размер минибуфера.

(setq message-truncate-lines nil)                  ; Длинные сообщения не обрезаются.

;; Настройка минималистичного вида минибуфера с иконками.

;; (use-package taoline
;;   :if window-system
;;   :after (all-the-icons)
;;   :functions (taoline-mode)
;;   :init (установить-из :repo "11111000000/taoline")
;;   :config
;;   (taoline-mode 1))

(require 'time)

(use-package doom-modeline
  :ensure t
  :functions (doom-modeline-mode)
  :config
  (doom-modeline-mode 1)
  (display-battery-mode 1)
  (display-time-mode 1)
  (setq display-time-format "%Y-%m-%d %H:%M")
  (setq display-time-default-load-average nil))

(use-package hide-mode-line
  :ensure t
  :functions (turn-off-hide-mode-line-mode
         turn-on-hide-mode-line-mode)
  :hook (((treemacs-mode
         eshell-mode shell-mode
         dired-mode
         term-mode vterm-mode
         embark-collect-mode
         lsp-ui-imenu-mode
         pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;;;; Иконки
;;;;; All The Icons

;; Подключение библиотеки для использования иконок в интерфейсе.

(use-package all-the-icons
  :if window-system
  :custom
  (all-the-icons-scale-factor 1)                   ; Настройка масштаба иконок.
  (all-the-icons-default-adjust 0)                 ; Настройка размещения иконок.
  :ensure t)

;;;;; Иконки для автодополнения

;; Подключение иконок для автодополнения с помощью `kind-icon`.

(use-package kind-icon
  :ensure t
  :after corfu
  :defines (corfu-margin-formatters)
  :functions (kind-icon-margin-formatter kind-icon-reset-cache)
  :custom
  (kind-icon-use-icons t)                           ; Включение использования иконок.
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Добавление форматировщика иконок.
  (add-hook 'kb/themes-hooks #'(lambda ()
                                (interactive)
                                (kind-icon-reset-cache))))  ; Обновление иконок при смене темы.

;; Подключение иконок для среды Marginalia.

(use-package nerd-icons-completion
  :ensure t
  :functions (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (unless (display-graphic-p) (nerd-icons-completion-mode))) ; Отключение иконок в терминальном режиме.

;;;;; Иконки Treemacs для Dired

;; Настройка иконок в `dired` (менеджер файлов) для более удобного интерфейса.

(use-package treemacs-icons-dired
  :ensure t
  :functions (treemacs-icons-dired-mode)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :init
  (add-hook 'after-load-theme-hook
           (lambda ()
             (treemacs-icons-dired-mode -1)  ; Отключение иконок после смены темы.
             (sleep-for 0)
             (treemacs-icons-dired-mode 1)))) ; Включение иконок после небольшой задержки.

;;;;; Иконки для ibuffer

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))  ; Включение иконок в ibuffer.

;;;; Хук при установке темы:

(defvar after-load-theme-hook nil
  "Хук, срабатывающий после установки темы `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
 "Запускает `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))  ; Запуск пользовательских хуков после установки темы.

;;;; Курсор

;; Настройка типа курсора - мигающий прямоугольник с возможностью изменения ширины.

(setq cursor-type '(bar . 3))                 ; Установка курсора

(setq x-stretch-cursor t)                      ; Распространение курсора по высоте символа.

;; В невыбранных окнах курсор будет полупрозрачным.

(setq-default cursor-in-non-selected-windows t)

;; Использование пакета для изменения курсора в зависимости от ввода.

(use-package cursor-chg
  :init (установить-из :repo "emacsmirror/cursor-chg")
  :functions (change-cursor-mode)
  :defines (curchg-default-cursor-color)
  :config
  (require 'cursor-chg)
  (setq-default curchg-input-method-cursor-color "orange"
           curchg-default-cursor-type '(bar . 3)
           curchg-default-cursor-color "forest green"
           curchg-change-cursor-on-input-method-flag t) ; Изменение курсора при смене метода ввода.
  (change-cursor-mode t))

;;;; Прокрутка

;;;;; Настройки прокрутки

;; Настройки для изменения поведения прокрутки.

(setq-default scroll-conservatively 80
         scroll-step 1
         scroll-margin 5
         hscroll-step 1
         auto-window-vscroll nil
         fast-but-imprecise-scrolling t
         jit-lock-defer-time 0
         hscroll-margin 1)

;;;;; Плавная прокрутка изображений

;; Включение плавной прокрутки для определённых режимов.

(use-package iscroll
  :ensure t
  :functions (iscroll-mode)
  :init
  (add-hook 'org-mode-hook #'iscroll-mode)
  (add-hook 'markdown-mode-hook #'iscroll-mode)
  (add-hook 'image-mode-hook #'iscroll-mode)
  (add-hook 'eww-mode-hook #'iscroll-mode)
  (add-hook 'w3m-mode-hook #'iscroll-mode))

;;;; Меню режима

;; Включение меню для текущего файла, например в Org-mode, для отображения заголовков.

(use-package imenu
  :custom ((imenu-auto-recsan t)))

;;;; Сокращение диалогов до y/n

;; Упрощение диалоговых окон до y/n вместо yes/no.

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Фон для служебных буферов

;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (solaire-global-mode t))  ; Включение цветного фона для служебных буферов.

;;;; Изображения

(require 'image-mode)           ; Подключение модуля для работы с изображениями.

(use-package image+
  :defer t
  :ensure t
  :after 'image-mode
  :hook (image-mode . image+)   ; Включение расширенного режима работы с изображениями.
  :bind ((:map image-mode-map
                 ("0" . imagex-sticky-restore-original)  ; Восстановление оригинала изображения.
                 ("+" . imagex-sticky-maximize)          ; Максимизация изображения.
                 ("=" . imagex-sticky-zoom-in)           ; Увеличение изображения.
                 ("-" . imagex-sticky-zoom-out))))       ; Уменьшение изображения.

;;;; Подсвечивать при перемещении
;; Подсвечивание текущей строки при перемещении курсора для улучшения визуального восприятия.
;; (defun pulse-line (&rest _)
;;   "Pulse the current line."
;;   (pulse-momentary-highlight-one-line (point)))


;; (dolist (command '(scroll-up-command
;;                   scroll-down-command
;;                   recenter-top-bottom
;;                   other-window
;;                   windmove-down
;;                   windmove-up
;;                   windmove-left
;;                   windmove-right
;;                   flymake-goto-next-error
;;                   flymake-goto-prev-error
;;                   xref-find-definitions
;;                   xref-find-definitions-other-window
;;                   goto-char))
;;   (advice-add command :after #'pulse-line))  ; Привязка функции подсветки к командам перемещения.

(use-package pulsar
  :disabled t
  :ensure t  
  :hook ((next-error xref-after-return) . pulsar-pulse-line) ; only pulse, don't recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump) . pulsar-recenter-center) ; pulse and recenter
  :hook ((consult-after-jump imenu-after-jump xref-after-jump xref-after-return) . pulsar-reveal-entry) ; reveal if hidden
  :custom
  (pulsar-face 'pulsar-green)
  :config
  (cl-callf append pulsar-pulse-functions
    '(what-cursor-position scroll-up-command scroll-down-command kill-whole-line yank-from-kill-ring yank yank-pop))
  (pulsar-global-mode -1))

;; Подсветка строки только в текущем окне

;; (defun подсветка-строки-только-в-текущем-окне ()
;;   "Enable hl-line-mode in the current window if god-mode is enabled or it is a dired buffer, and disable it in all other windows."
;;   (let ((active-window (selected-window)))  ; Сохраняем текущее активное окно
;;     ;; Отключаем hl-line-mode во всех неактивных окнах
;;     (dolist (win (window-list))
;;       (with-selected-window win
;;         (hl-line-mode -1)))  ; Выключаем в каждом окне

;;     ;; Включаем hl-line-mode в активном окне, если god-mode включен или это dired
;;     (with-selected-window (selected-window)
;;       (when (or god-local-mode ; Проверяем, активен ли god-mode
;;                buffer-read-only ; Толькочтение 
;;                (derived-mode-p 'dired-mode)) ; или dired
;;         (hl-line-mode 1)))))

;; (add-hook 'window-selection-change-functions (lambda (_) (подсветка-строки-только-в-текущем-окне)))


;;;; подтверждение выключения процессов

(setq-default confirm-kill-processes nil)  ; Отключение подтверждения перед завершением процессов.

;;;; Красивые индикаторы на рамке

(use-package modern-fringes
  :ensure t
  :defines (modern-fringes-mode)
  :config
  (modern-fringes-mode t))  ; Включение кастомизированных индикаторов.

;;;; Выделение текущего окна
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))

(provide 'про-внешний-вид)  ; Экспортирование конфигурации для использования в других частях Emacs.
;;; про-внешний-вид.el ends here
