;;; про-внешний-вид.el --- Внешний вид и интерфейс Emacs -*- lexical-binding: t -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: ui, appearance, icons, tabs
;; URL: https://example.com/про-внешний-вид
;;
;;; Commentary:
;;
;; Этот файл настраивает визуальный интерфейс Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция логически объясняется, мотивируется и
;; связывается с остальными.  Мы стремимся к элегантности,
;; минимализму и производительности в лучших традициях Emacs — с
;; использованием `use-package` для декларативной конфигурации,
;; хуков для автоматизации и отложенной загрузки для скорости.
;;
;; Визуальный комфорт — ключ к продуктивности.  Здесь
;; мы очищаем интерфейс от отвлекающих элементов, добавляем иконки для
;; интуитивности, улучшаем навигацию (курсор, прокрутка, вкладки) и
;; интегрируем мелочи, делающие Emacs современным и приятным.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и установки.
;; 1. Базовый интерфейс: Минимализм — скрытие лишнего, отключение сигналов.
;; 2. Минибуфер и статус: Улучшенный центр ввода с временем/батареей.
;; 3. Иконки: Эстетика в автодополнении, dired, ibuffer и т.д.
;; 4. Курсор и прокрутка: Выразительный курсор, плавная навигация.
;; 5. Вкладки: Современный tab-bar и tab-line с иконками.
;; 6. Интеграция с текстовым режимом: Подключение TTY оптимизаций.
;; 7. Дополнительные улучшения: Разделители, изображения, fringes.
;; 8. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-внешний-вид) в вашем init.el.
;; Рекомендуется интегрировать с темами (например, через modus-themes) для
;; полной гармонии. Если вы в терминале, некоторые фичи (иконки) отключатся
;; автоматически для стабильности.
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные
;; хуки и минимальные глобальные изменения. Закомментированные секции —
;; опции для экспериментов.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем утилиты для установки пакетов. `установить-из`
;; — кастомная функция для загрузки из репозиториев, что позволяет
;; легко добавлять нестандартные пакеты без ELPA.

(require 'установить-из)

;;;; 1. Базовый интерфейс
;; Начинаем с гигиены: убираем отвлекающие элементы (бары, сигналы),
;; чтобы фокус был на контенте. Это базовый шаг к минималистичному
;; Emacs, где ничто не мешает творчеству.

;; Отключаем раздражители: нет звукам, визуальным колокольчикам.
(setq visible-bell nil ring-bell-function 'ignore)

;; Скрываем ненужные бары: скролл, горизонтальный скролл.
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

;; Разделители окон: тонкие линии для визуального разделения,
;; только в графическом режиме для эстетики.
(when (display-graphic-p)
  (setq window-divider-default-bottom-width 1 window-divider-default-places 'bottom-only)
  (window-divider-mode 1))

;;;; 2. Минибуфер и статус
;; Минибуфер — сердце ввода в Emacs. Мы делаем его гибким, добавляем
;; полезные индикаторы (время, батарея) и элегантную строку статуса.
;; Это улучшает осведомлённость без отвлечения.

(setq-default enable-recursive-minibuffers t            ; Разрешаем вложенные минибуферы.
              max-mini-window-height 0.33               ; Ограничение по высоте для компактности.
              message-truncate-lines nil                ; Полные сообщения без усечения.
              resize-mini-windows t)                    ; Авто-адаптация размера.

;; Строка статуса с иконками (shaoline): минималистичный и информативный.
(add-to-list 'load-path "/home/az/.emacs.d/elpa/shaoline/lisp")

(use-package shaoline
  :ensure t
  :init (установить-из :repo "11111000000/shaoline")
  :custom
  (shaoline-debug nil)
  (shaoline-mode-strategy 'yang)
  (shaoline-with-tray t)
  (shaoline-enable-dynamic-segments t)
  (shaoline-preserve-modeline-modes
   '(help-mode
     context-navigator-view-mode))
  :hook (after-init . shaoline-mode))

(require 'time)

;;;; 3. Иконки
;; Иконки добавляют визуальную интуицию: в автодополнении, файловых
;; менеджерах и списках. Мы интегрируем их последовательно, с учётом
;; графического/терминального режимов, и сбрасываем кэш при смене тем.

;; Иконки загружаем только в графическом режиме для стабильности.
(when (display-graphic-p)

  ;; Иконки в автодополнении (kind-icon): рядом с кандидатами в corfu.
  (use-package kind-icon
    :defer t
    :ensure t
    :after corfu
    :custom (kind-icon-use-icons t)
    (kind-icon-default-face 'corfu-default)
    :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    ;; Сброс кэша иконок после смены темы.
    (add-hook 'after-load-theme-hook #'kind-icon-reset-cache))

  ;; Иконки в marginalia (подсказки) через nerd-icons-completion.
  ;; (use-package nerd-icons-completion
  ;;   :defer t
  ;;   :ensure t
  ;;   :after marginalia
  ;;   :disabled t
  ;;   :config
  ;;   (nerd-icons-completion-mode)
  ;;   (nerd-icons-completion-marginalia-setup))

  ;; Иконки в dired (файловый менеджер) через treemacs.
  (use-package treemacs-icons-dired
    :defer t
    :ensure t
    :hook (dired-mode . treemacs-icons-dired-enable-once)
    :config (add-hook 'after-load-theme-hook (lambda ()
                                               (treemacs-icons-dired-mode -1)
                                               (treemacs-icons-dired-mode 1))))

  ;; Иконки в ibuffer (список буферов).
  (use-package nerd-icons-ibuffer
    :defer t
    :ensure t
    :hook (ibuffer-mode . nerd-icons-ibuffer-mode)))

;;;; 4. Курсор и прокрутка
;; Курсор — ваш "палец" в Emacs. Мы делаем его выразительным (тип, цвет),
;; а прокрутку — плавной и отзывчивой, чтобы навигация ощущалась естественно.

;; Базовые настройки курсора: бар шириной 3, растягивается по символу.
(setq cursor-type '(bar . 3) x-stretch-cursor t cursor-in-non-selected-windows t)

;; Динамический курсор: меняет тип/цвет по контексту (только для GUI).
(when (display-graphic-p)
  (use-package cursor-chg
    :defer t
    :init (установить-из :repo "emacsmirror/cursor-chg")
    :hook (after-init . change-cursor-mode)
    :custom (curchg-input-method-cursor-color "orange")
    (curchg-default-cursor-type '(bar . 3))
    (curchg-default-cursor-color "forest green")
    (curchg-change-cursor-on-input-method-flag t)))

;; Прокрутка: консервативная, с margins для комфорта.
(setq-default scroll-conservatively 80 scroll-step 1 scroll-margin 5 hscroll-step 1 auto-window-vscroll nil fast-but-imprecise-scrolling t jit-lock-defer-time 0 hscroll-margin 1)

;; Плавная прокрутка: особенно эффективна в GUI режиме.
(use-package iscroll
  :defer t
  :ensure t
  :hook ((org-mode markdown-mode image-mode eww-mode w3m-mode) . iscroll-mode)
  :config
  ;; В TTY режиме используем более простые настройки прокрутки
  (unless (display-graphic-p)
    (setq iscroll-preserve-screen-position t)))

;;;; 5. Вкладки
;; Вкладки — современный способ организации: tab-bar для глобальных,
;; tab-line для буферов в окне. Мы используем pro-tabs для унификации
;; с иконками и удобными биндингами.

(use-package pro-tabs
  :init (установить-из :repo "11111000000/pro-tabs")
  :bind (;; Глобальные бинды для tab-bar.
         ("s-n" . tab-bar-switch-to-next-tab)
         ("s-p" . tab-bar-switch-to-prev-tab)
         ("s-w" . tab-bar-close-tab)
         :map tab-bar-mode-map
         ("s-n" . tab-bar-switch-to-next-tab)
         ("s-p" . tab-bar-switch-to-prev-tab)
         ("s-<tab>" . tab-bar-switch-to-next-tab)
         ("S-s-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
         ("s-w" . tab-bar-close-tab)
         :map tab-line-mode-map ("s-<tab>" . tab-line-switch-to-next-tab)
         ("S-s-<iso-lefttab>" . tab-line-switch-to-prev-tab)
         ("C-w" . pro/tab-line-close-tab))
  :custom
  ;; Иконки включаем только в графическом режиме
  (pro-tabs-enable-icons (display-graphic-p))
  :hook ((after-init-hook . pro-tabs-mode))
  :init
  (tab-bar-mode t))

;; ;;;; 6. Интеграция с текстовым режимом
;; ;; Подключаем специализированные настройки для TTY режима.
;; ;; Отдельный модуль про-текстовый-режим обрабатывает все терминальные
;; ;; особенности, оставляя этот файл сфокусированным на GUI элементах.

;; (when (not (display-graphic-p))
;;   (require 'про-текстовый-режим nil t))

;;;; 7. Дополнительные улучшения
;; Здесь — мелочи, завершающие картину: изображения, fringes, диалоги.

;; imenu: авто-ресканирование для оглавлений в больших файлах.
(use-package imenu
  :custom (imenu-auto-rescan t))

;; Короткие диалоги: y/n вместо yes/no.
(defalias 'yes-or-no-p 'y-or-n-p)

;; GUI-специфичные улучшения: изображения, fringes, визуальные эффекты.
(when (display-graphic-p)

  ;; Интерактивные изображения: масштабирование в image-mode.
  (use-package image+
    :ensure t
    :bind (:map image-mode-map ("0" . imagex-sticky-restore-original)
                ("+" . imagex-sticky-maximize)
                ("=" . imagex-sticky-zoom-in)
                ("-" . imagex-sticky-zoom-out)))

  ;; Современные fringes: цветные индикаторы для сплитов.
  (use-package modern-fringes
    :defer t
    :ensure t
    :hook (after-init . modern-fringes-mode))

  ;; Prettify utils: база для многих визуальных пакетов.
  (use-package prettify-utils
    :defer t
    :init (установить-из :repo "Ilazki/prettify-utils.el"))

  ;; Цветной фон для служебных буферов (только в GUI).
  (use-package solaire-mode
    :defer t
    :ensure t
    :hook (after-init . solaire-global-mode)))

;;;; Текстовые меню - Transient

(require 'установить-из)

;; (use-package transient
;;   :init (установить-из :repo "magit/transient" :name "transient"))

                                        ;(add-to-list 'package--builtin-versions '(transient 0 4 3))

(use-package transient-posframe
  :ensure t
  :config
  (transient-posframe-mode t))

;; Универсальные визуальные улучшения для всех режимов.

;; Новое: показывать относительные номера строк в буферах с кодом.
(use-package display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-type t)
  (display-line-numbers-width-start t))

(when (display-graphic-p)
  ;; Базовая поддержка изображений везде
  (setq image-animate-loop t))

;; Хук для тем: запускать кастомные действия после load-theme.
(defvar after-load-theme-hook nil "Хук после (load-theme).")
(defadvice load-theme (after run-after-load-theme-hook activate) "Запуск after-load-theme-hook." (run-hooks 'after-load-theme-hook))

;;;; 8. Финал
;; Завершаем: не беспокоим о процессах при выходе, предоставляем модуль.

;; Запретить выход по C-x C-c; вместо этого использовать C-x M-q
(defun pro/quit-emacs ()
  "Сохранить все буферы и выйти из Emacs без лишних вопросов."
  (interactive)
  (save-buffers-kill-emacs))

;; Вежливо предупреждать при попытке выйти по C-x C-c
(defun pro/block-quit-c-x-c-c ()
  "Блокировать выход по C-x C-c."
  (interactive)
  (user-error "Выход отключён: используйте C-x M-q"))

(setq confirm-kill-processes nil
      confirm-kill-emacs nil)

;; Переназначения клавиш выхода
(global-set-key (kbd "C-x C-c") #'pro/block-quit-c-x-c-c)
(global-set-key (kbd "C-x M-q") #'pro/quit-emacs)

(provide 'про-внешний-вид)
;;; про-внешний-вид.el ends here
