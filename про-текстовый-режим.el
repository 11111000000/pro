;;; про-текстовый-режим.el --- Настройки для текстового режима TTY -*- lexical-binding: t -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: tty, terminal, keyboard, console
;; URL: https://example.com/про-текстовый-режим
;;
;;; Commentary:
;;
;; Этот файл содержит специализированные настройки для работы Emacs
;; в текстовом режиме (TTY/консоль). Здесь мы оптимизируем интерфейс
;; под ограничения терминала, настраиваем клавиатуру (включая превращение
;; Caps Lock в Ctrl), улучшаем отображение и производительность.
;;
;; Философия: терминальный режим не должен быть компромиссом. Мы создаём
;; полнофункциональную среду разработки, максимально используя возможности
;; современных терминалов и компенсируя отсутствие GUI элементов умными
;; альтернативами.
;;
;; Структура файла:
;; 1. Определение TTY и активация: Проверка режима и условная загрузка
;; 2. Клавиатурные настройки: Caps как Ctrl, улучшенная поддержка клавиш
;; 3. Визуальные адаптации: Курсор, цвета, псевдографика
;; 4. Производительность: Оптимизация перерисовки и скроллинга
;; 5. Мышь и навигация: xterm-mouse, улучшенное взаимодействие
;; 6. Альтернативы GUI элементам: Замены иконок, упрощённый интерфейс
;; 7. Универсальные улучшения: Настройки, полезные и в TTY, и в GUI
;;
;; Использование: Автоматически загружается при работе в TTY режиме.
;; Можно также принудительно загрузить через (require 'про-текстовый-режим).
;; Не влияет на GUI режим - все настройки условные.

;;; Code:

;;;; 1. Определение TTY и активация
;; Проверяем, работаем ли мы в текстовом режиме, и активируем
;; настройки только для TTY, оставляя GUI нетронутым.

(defun текстовый-режим-p ()
  "Проверить, работает ли Emacs в текстовом режиме."
  (not (display-graphic-p)))

(when (текстовый-режим-p)

  ;;;; 2. Клавиатурные настройки
  ;; Самое важное в TTY - правильно настроенная клавиатура.
  ;; Caps Lock как Ctrl, улучшенная поддержка функциональных клавиш.

  ;; Caps Lock работает как Ctrl (требует настройки на уровне системы).
  ;; Для X11: setxkbmap -option ctrl:nocaps
  ;; Для Wayland: добавить в ~/.xkb или использовать gsettings
  ;; Для console: sudo dumpkeys | sed 's/Caps_Lock/Control_L/g' | sudo loadkeys

  ;; Альтернативный способ - через input-decode-map для некоторых терминалов
  ;;;(define-key input-decode-map [?\C-m] [C-m])

  ;; Улучшенная поддержка Meta клавиш в TTY
  (setq meta-prefix-char nil)

  ;; Расширенная поддержка функциональных клавиш
  (when (getenv "TERM")
    (cond
     ;; xterm и совместимые
     ((string-match "^xterm" (getenv "TERM"))
      (define-key input-decode-map "\e[1;5C" [C-right])
      (define-key input-decode-map "\e[1;5D" [C-left])
      (define-key input-decode-map "\e[1;3C" [M-right])
      (define-key input-decode-map "\e[1;3D" [M-left]))

     ;; tmux специфичные настройки
     ((string-match "^tmux\\|^screen" (getenv "TERM"))
      (define-key input-decode-map "\e[1~" [home])
      (define-key input-decode-map "\e[4~" [end]))))

  ;;;; 3. Визуальные адаптации
  ;; Адаптируем отображение под ограничения терминала:
  ;; блочный курсор, оптимизированные цвета, псевдографика.

  ;; Курсор в TTY: яркий блочный курсор лучше виден
  (setq visible-cursor t
        cursor-type 'box
        cursor-in-non-selected-windows 'hollow)

  ;; Цвета терминала: максимальная поддержка палитры
  (setq-default xterm-color-preserve-properties t
                term-suppress-hard-newline t)

  ;; Включаем поддержку 256 цветов если доступно
  (when (getenv "TERM")
    (when (string-match "256color\\|truecolor" (getenv "TERM"))
      (setq frame-background-mode 'dark)))

  ;; Псевдографические разделители окон
  (setq window-divider-default-bottom-width 1
        window-divider-default-right-width 1)

  ;; Улучшенное отображение Unicode в терминале
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  ;;;; 4. Производительность
  ;; Оптимизируем перерисовку и отзывчивость для терминального режима.

  ;; Быстрая перерисовка без потери качества
  (setq fast-but-imprecise-scrolling t
        redisplay-skip-fontification-on-input t
        jit-lock-defer-time 0.1)

  ;; Отключаем ненужные в TTY функции для экономии ресурсов
  (setq use-file-dialog nil
        use-dialog-box nil
        inhibit-startup-screen t)

  ;; Оптимизируем GC для терминального режима
  (setq gc-cons-threshold (* 20 1024 1024)) ; 20MB

  ;;;; 5. Мышь и навигация
  ;; Включаем поддержку мыши в терминале, улучшаем навигацию.

  ;; Поддержка мыши в xterm-совместимых терминалах
  (unless (and (boundp 'xterm-mouse-mode) xterm-mouse-mode)
    (xterm-mouse-mode 1))

  ;; Поддержка колёсика мыши
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)

  ;; Улучшенная поддержка трекпада (если доступна)
  (setq mouse-wheel-tilt-scroll t)

  ;;;; 6. Альтернативы GUI элементам
  ;; Заменяем графические элементы текстовыми аналогами.

  ;; Упрощённая mode-line оптимизированная для TTY
  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
                               display (min-width (5.0)))
                  "  "
                  (:propertize ("" mode-line-frame-identification mode-line-buffer-identification)
                               face mode-line-buffer-id)
                  "   "
                  mode-line-position
                  (vc-mode vc-mode)
                  "  "
                  mode-line-modes
                  mode-line-misc-info
                  mode-line-end-spaces))

  ;; Альтернативы иконкам - используем символы Unicode/ASCII
  (setq completion-show-inline-help t
        completions-detailed t)

  ;; Улучшенные popup и tooltip для TTY
  (setq tooltip-mode nil
        help-window-select t)

  ;;;; 7. Специфичные для TTY пакеты и настройки
  ;; Пакеты и функции, которые особенно полезны в текстовом режиме.

  ;; Усиленная поддержка цветов в терминале
  (when (require 'xterm-color nil t)
    (setq comint-output-filter-functions
          (remove 'ansi-color-process-output comint-output-filter-functions))
    (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter))

  ;; Подсказка контекста для навигации без мыши
  (setq which-key-popup-type 'side-window
        which-key-side-window-location 'bottom)

  ;; Улучшенная поддержка буфера *scratch* в TTY
  (setq initial-scratch-message
        ";; TTY режим активен. Caps Lock работает как Ctrl.\n;; Доступна поддержка мыши и 256 цветов.\n\n"))

;;;; 8. Универсальные улучшения
;; Настройки, полезные как в TTY, так и в GUI режимах.

(when (текстовый-режим-p)
  ;; Режим подсветки парных скобок
  (show-paren-mode 1)
  (setq show-paren-delay 0
        show-paren-style 'parenthesis)

  ;; Подсветка текущей строки
  (global-hl-line-mode 1)

  ;; Относительные номера строк (особенно полезны в TTY для навигации)
  (when (version<= "26.0.50" emacs-version)
    (global-display-line-numbers-mode)
    (setq-default display-line-numbers-type 'relative
                  display-line-numbers-width-start t))

  ;; Показ номера колонки
  (column-number-mode 1)

  ;; Стандартные настройки табуляции
  (setq-default tab-width 4
                indent-tabs-mode nil)

  ;; Альтернативы fringes в TTY - используем символы
  (setq-default indicate-buffer-boundaries 'left
                indicate-empty-lines t)

  ;; Базовая поддержка изображений везде (не мешает, даже если нет inline graphics)
  (setq image-animate-loop t)

  ;; Улучшенная поддержка whitespace в TTY
  (setq whitespace-style '(face tabs spaces trailing space-before-tab
                                newline indentation empty space-after-tab)
        whitespace-display-mappings
        '((space-mark 32 [183] [46])     ; обычный пробел
          (space-mark 160 [164] [95])    ; неразрывный пробел
          (newline-mark 10 [36 10])      ; конец строки
          (tab-mark 9 [8594 9] [92 9]))) ; табуляция

  ;; Короткие диалоги везде
  (defalias 'yes-or-no-p 'y-or-n-p))

;;;; 9. Хуки и автоматизация
;; Автоматические действия для поддержания оптимального состояния TTY.

(when (текстовый-режим-p)
  ;; Хук для оптимизации после смены буфера
  (add-hook 'buffer-list-update-hook
            (lambda ()
              (when (текстовый-режим-p)
                ;; Принудительная перерисовка для стабильности
                (redraw-display))))

  ;; Хук для восстановления настроек после suspend/resume
  (add-hook 'suspend-resume-hook
            (lambda ()
              (when (текстовый-режим-p)
                ;; Переинициализируем mouse mode
                (xterm-mouse-mode -1)
                (xterm-mouse-mode 1)
                ;; Восстанавливаем кодировку
                (set-terminal-coding-system 'utf-8)))))

;;;; 10. Вспомогательные функции
;; Утилиты для работы в TTY режиме.

(defun tty-optimize ()
  "Оптимизировать Emacs для текстового режима."
  (interactive)
  (when (текстовый-режим-p)
    (message "Оптимизация TTY режима...")
    ;; Очистка экрана
    (redraw-display)
    ;; Сброс мыши
    (xterm-mouse-mode -1)
    (xterm-mouse-mode 1)
    ;; Восстановление кодировки
    (set-terminal-coding-system 'utf-8)
    (message "TTY оптимизирован!")))

(defun tty-info ()
  "Показать информацию о текущем терминальном режиме."
  (interactive)
  (if (текстовый-режим-p)
      (message "TTY режим: %s | Цвета: %s | Мышь: %s"
               (getenv "TERM")
               (if (display-color-p) "есть" "нет")
               (if xterm-mouse-mode "включена" "выключена"))
    (message "Работаем в графическом режиме")))

;; Удобная клавиша для оптимизации TTY
(when (текстовый-режим-p)
  (global-set-key (kbd "C-c t o") #'tty-optimize)
  (global-set-key (kbd "C-c t i") #'tty-info))

;;;; 11. Финал

(provide 'про-текстовый-режим)
;;; про-текстовый-режим.el ends here
