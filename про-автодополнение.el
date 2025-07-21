;;; про-автодополнение.el --- Автодополнение в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: completion, corfu, cape, popup
;; URL: https://example.com/про-автодополнение
;;
;;; Commentary:
;;
;; Этот файл настраивает систему автодополнения в Emacs на базе Corfu,
;; следуя принципам литературного программирования: код представлен
;; как повествование, где каждая секция мотивируется, объясняется и
;; логически связывается с остальными. Мы стремимся к элегантности, минимализму и
;; производительности в лучших традициях Emacs — с использованием `use-package`
;; для декларативной конфигурации, хуков для автоматизации и отложенной загрузки
;; для скорости.
;;
;; Почему это важно? Автодополнение ускоряет ввод кода и текста, снижая ошибки
;; и повышая продуктивность. Здесь мы интегрируем Corfu (лёгкий popup) с Cape
;; (расширениями для различных источников), обеспечивая глобальное, но ненавязчивое
;; поведение. Это делает Emacs более интуитивным, как в современных IDE, но без
;; излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и утилиты.
;; 1. Инициализация Corfu: Основной UI для автодополнения.
;; 2. Настройка для минибуфера: Условное включение, совместимость с Vertico.
;; 3. Отключение тормозящих CAPF: Убираем ispell для производительности.
;; 4. Расширения Cape: Дополнительные источники автодополнения.
;; 5. Автодополнение в терминале: Corfu-terminal для не-графических сессий.
;; 6. Опциональные расширения: Закомментированные предпросмотры и оверлеи.
;; 7. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-автодополнение) в вашем init.el.
;; Рекомендуется интегрировать с Vertico/Orderless для полного стека. Закомментированные
;; секции — опции для экспериментов (например, предпросмотр кандидатов).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Для конфликтов — проверяйте capf.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые пакеты. `use-package` обеспечивает ленивую
;; загрузку, а `corfu` и друзья — основу. Мы предполагаем наличие Vertico/Mct
;; из других модулей для совместимости.

(require 'use-package)

;;;; 1. Инициализация Corfu
;; Corfu — это лёгкий, popup-based интерфейс автодополнения. Мы настраиваем
;; его глобально, с кастомными биндами для навигации и вставки, и опциями
;; для автоматического срабатывания. Это основа: быстро, ненавязчиво, интегрируется
;; с capf (completion-at-point-functions).

(use-package corfu
  :ensure t
  :defines (corfu-map)
  :functions (corfu-mode global-corfu-mode corfu-popupinfo-mode corfu-history-mode)
  :bind
  (:map corfu-map
        ("<escape>" . corfu-quit)
        ("<return>" . corfu-insert)
        ("SPC" . (lambda () (interactive) (progn (corfu-insert) (corfu-insert-separator))))
        ("C-h" . corfu-info-documentation)
        ("M-l" . corfu-info-location)
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)           ; TAB всегда пытается дополнить.
  (completion-cycle-threshold nil)        ; Нет автопереключения кандидатов.
  (corfu-auto t)                          ; Автоматическое срабатывание.
  (corfu-auto-prefix 3)                   ; После 3 символов.
  (corfu-auto-delay 0.4)                  ; Задержка 0.4с.
  (corfu-popupinfo-delay 0.5)             ; Задержка инфо-попапа.
  (corfu-min-width 5)                     ; Минимальная ширина попапа.
  (corfu-max-width 70)                    ; Максимальная ширина.
  (corfu-count 14)                        ; Максимум 14 кандидатов.
  (corfu-scroll-margin 3)                 ; Маржа прокрутки.
  (corfu-cycle t)                         ; Циклическая навигация.
  (corfu-echo-documentation nil)          ; Не эхоить документацию.
  (corfu-separator ?\s)                   ; Разделитель кандидатов.
  (corfu-quit-no-match 'separator)        ; Выход, если нет совпадений.
  (corfu-preview-current nil)             ; Нет предпросмотра текущего.
  (corfu-preselect 'prompt)               ; Предвыбор в промпте.
  :config
  (global-corfu-mode t)                   ; Глобальное включение.
  (corfu-popupinfo-mode t)                ; Инфо-попапы.
  (corfu-history-mode t))                 ; История для повторного использования.

;;;; 2. Настройка для минибуфера
;; В минибуфере Corfu должен включаться только если нет Vertico/Mct —
;; это обеспечивает совместимость. Мы определяем функцию-хук для
;; условного включения, чтобы не конфликтовать с другими UI.

(defun в-минибуфере-включать-corfu ()
  "Включать Corfu в минибуфере если Vertico/Mct не активны."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)           ; Локально отключаем авто.
    (corfu-mode 1)))

(add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1)

;;;; 3. Отключение тормозящих CAPF
;; Ispell-completion-at-point может тормозить, особенно в коде/тексте.
;; Мы убираем его из capf глобально, через хуки для prog/text-mode.
;; Альтернатива — cape-dict, закомментирована для опций.

(defun pro/disable-ispell-capf ()
  "Убрать `ispell-completion-at-point' из списка CAPF."
  (setq-local completion-at-point-functions
              (remq #'ispell-completion-at-point
                    completion-at-point-functions)))

(add-hook 'text-mode-hook #'pro/disable-ispell-capf)
(add-hook 'prog-mode-hook #'pro/disable-ispell-capf)

;; Альтернатива: cape-dict для словарного автодополнения (раскомментируйте).
;; (with-eval-after-load 'cape
;;   (add-to-list 'completion-at-point-functions #'cape-dict))

;;;; 4. Расширения Cape
;; Cape предоставляет дополнительные capf для источников (файлы, словари и т.д.).
;; Мы инициализируем его просто, без лишних настроек — он интегрируется с Corfu.

(use-package cape
  :ensure t
  :init)

;;;; 5. Автодополнение в терминале
;; Corfu-terminal адаптирует Corfu для не-графических (TTY) сессий,
;; используя overlay вместо попапов. Это обеспечивает единый опыт везде.

(use-package corfu-terminal
  :ensure t
  :hook (after-init . corfu-terminal-mode))  ; Включаем глобально после init.

;;;; 6. Опциональные расширения
;; Здесь закомментированные опции: оверлей первого кандидата и предпросмотр
;; дополнения. Они могут быть полезны для экспериментов, но отключены по умолчанию
;; для минимализма. Если нужны — раскомментируйте и настройте хуки.

;; ;; Оверлей первого кандидата (corfu-candidate-overlay): предпросмотр inline.
;; (use-package corfu-candidate-overlay
;;   :ensure t
;;   :after corfu
;;   :defines (corfu-candidate-overlay-map corfu-mode-map)
;;   :functions (corfu-candidate-overlay-mode)
;;   :bind (:map corfu-mode-map
;;               ("M-TAB" . corfu-candidate-overlay-complete-at-point))
;;   :hook ((emacs-lisp-mode . corfu-candidate-overlay-mode)
;;          (typescript-ts-mode . corfu-candidate-overlay-mode)
;;          (js-ts-mode . corfu-candidate-overlay-mode))
;;   :config
;;   ;; (global-set-key (kbd "M-<tab>") 'completion-at-point)
;;   )

;; ;; Предпросмотр дополнения (completion-preview): inline-подсказки.
;; (use-package completion-preview
;;   :ensure nil
;;   :functions (completion-preview-insert-word completion-preview-insert-sexp)
;;   :bind (:map completion-preview-active-mode-map
;;               ("M-f" . #'completion-preview-insert-word)
;;               ("C-M-f" . #'completion-preview-insert-sexp))
;;   :custom
;;   (completion-preview-minimum-symbol-length 3)
;;   :init
;;   (add-hook 'text-mode-hook (lambda () (completion-preview-mode -1)))
;;   (global-completion-preview-mode))

;;;; 7. Финал
;; Завершаем модуль стандартным provide.

(provide 'про-автодополнение)
;;; про-автодополнение.el ends here
