;;; про-быстрый-доступ.el --- Быстрый поиск и доступ в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.2
;; Keywords: completion, search, consult, vertico, orderless
;; URL: https://example.com/про-быстрый-доступ
;;
;;; Commentary:
;;
;; Этот файл настраивает инструменты для быстрого поиска и доступа в Emacs,
;; следуя принципам литературного программирования: код представлен
;; как повествование, где каждая секция мотивируется, объясняется и
;; логически связывается с остальными.  Мы стремимся к элегантности, минимализму и
;; производительности в лучших традициях Emacs — с использованием `use-package`
;; для декларативной конфигурации, хуков для автоматизации и отложенной загрузки
;; для скорости.
;;
;; Почему это важно? В Emacs продуктивность зависит от скорости навигации и поиска.
;; Здесь мы интегрируем вертикальные списки (Vertico), гибкую сортировку (Orderless),
;; аннотации (Marginalia) и мощный Consult для унифицированного опыта. Это делает
;; Emacs более интуитивным, как современный IDE, но без излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и утилиты.
;; 1. Вертикальные списки: Основной UI для автодополнения (Vertico).
;; 2. Сортировка и аннотации: Гибкий поиск (Orderless) и подсказки (Marginalia).
;; 3. Consult и расширения: Универсальный поиск по буферам, файлам, коду.
;; 4. Поиск по файлам и проектам: Интеграция с Dired и Ag.
;; 5. Поиск по коду: Eglot и языковые серверы.
;; 6. Кастомные функции: Улучшенное переключение буферов с учётом вкладок и recentf.
;; 7. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-быстрый-доступ) в вашем init.el.
;; Рекомендуется интегрировать с Projectile для проектов. Закомментированные
;; секции — опции для экспериментов (например, ctrlf для поиска в файле).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Для конфликтов клавиш — проверяйте remap.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые пакеты. Projectile необходим для работы с проектами,
;; а cl-lib, seq, recentf и consult — для кастомных функций. Мы используем
;; `use-package` для всех внешних пакетов, обеспечивая ленивую загрузку.

(require 'projectile)
(require 'cl-lib)
(require 'seq)
(require 'recentf)
(require 'consult)

;;;; 1. Вертикальные списки
;; Vertico — это основа быстрого доступа: вертикальный UI для минибуфера.
;; Мы включаем игнор регистра, циклическую навигацию и кастомные бинды для
;; удобства. Это делает выбор элементов интуитивным, как в современных приложениях.

(use-package vertico
  :ensure t
  :functions (vertico-mode)
  :custom
  (read-file-name-completion-ignore-case t)  ; Игнор регистра для файлов.
  (read-buffer-completion-ignore-case t)     ; Игнор для буферов.
  (vertico-cycle t)                           ; Циклическая навигация.
  (completion-ignore-case t)                  ; Глобальный игнор регистра.
  :bind
  (:map vertico-map
        ("s-<tab>" . vertico-next)
        ("s-<iso-lefttab>" . vertico-previous))
  :init (vertico-mode t))  ; Раннее включение для глобального эффекта.

;; Опционально: поддержка директорий в Vertico (раскомментируйте, если нужно).
;; (use-package vertico-directory
;;   :ensure t
;;   :after vertico
;;   :bind
;;   (:map vertico-map
;;         ("RET" . vertico-directory-enter)
;;         ("DEL" . vertico-directory-delete-char))
;;   :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; Опционально: отображение в табличном виде (grid).
;; (use-package vertico-grid
;;   :ensure t
;;   :after vertico)

;;;; 2. Сортировка и аннотации
;; Orderless предоставляет гибкую, неупорядоченную сортировку — идеально для
;; fuzzy-поиска. Marginalia добавляет аннотации (подсказки) к кандидатам, делая
;; выбор осмысленным. Это комбо превращает минибуфер в мощный инструмент.

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :ensure t
  :functions (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode t))  ; Глобальное включение.

;;;; 3. Consult и расширения
;; Consult — это Swiss Army knife для поиска: по буферам, линиям, импорту и т.д.
;; Мы remap стандартные команды на Consult-аналоги, добавляем бинды и кастомы.
;; Это унифицирует интерфейс, делая Emacs более coherent.

(use-package consult
  :ensure t
  :functions (consult-customize consult-xref)
  :bind
  (;; Глобальные remap для стандартных команд.
   ([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
   :map consult-narrow-map
   ("C-h" . consult-narrow-help)
   :map goto-map
   ("m" . consult-mark)
   ("o" . consult-outline)
   :map help-map
   ("M" . consult-minor-mode-menu))
  :custom
  (consult-preview-key "M-.")
  (consult-line-start-from-top t)
  (xref-show-definitions-function #'consult-xref)
  (xref-show-xrefs-function #'consult-xref)
  :config
  (autoload 'projectile-project-root "projectile")
  (require 'consult-xref)
  (setq consult-project-function #'projectile-project-root))

;; Расширения Consult: для сниппетов и документации (Dash).
(use-package consult-yasnippet
  :ensure t)

(use-package consult-dash
  :ensure t
  :config
  (consult-customize consult-dash :initial (thing-at-point 'symbol)))

;;;; 4. Поиск по файлам и проектам
;; Здесь мы интегрируем поиск по файлам: через Consult-grep в Dired и Ag для
;; проектов. Это позволяет быстро находить текст в директориях, мотивируя
;; использование Emacs как файлового менеджера.

(require 'dired)

(defun искать-по-файлам-отсюда ()
  "Искать по файлам от текущей директории через Consult-grep."
  (interactive)
  (consult-grep default-directory))

(use-package consult-ag
  :ensure t
  :after dired
  :functions (consult-ag)
  :bind (:map dired-mode-map
              ("s" . искать-по-файлам-отсюда))
  :config
  ;; Кастомная функция для маркеров (из оригинала, для совместимости).
  (defun consult--position-marker (buffer line column)
    "Вернуть маркер в BUFFER на LINE и COLUMN."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (save-excursion
            (widen)
            (goto-char (point-min))
            (ignore-errors
              (forward-line (1- line))
              (forward-char column))
            (point-marker)))))))

;;;; 5. Поиск по коду
;; Интеграция с языковыми серверами: Consult-eglot для символов в Eglot.
;; Это связывает поиск с LSP, делая Emacs мощным для программирования.
;; (Consult-lsp закомментирован как отключённый вариант.)

;; (use-package consult-lsp
;;   :ensure t
;;   :after (lsp consult)
;;   :defer t
;;   :disabled t)

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :functions (consult-eglot-symbols)
  :bind (:map eglot-mode-map
              ("C-c C-." . consult-eglot-symbols)))

;;;; 6. Кастомные функции
;; Эти функции улучшают переключение буферов: учитывают вкладки, EXWM и recentf.
;; Они мотивированы необходимостью плавной навигации в многотабовой среде,
;; интегрируя Consult для выбора.

(defun pro/consult-buffer ()
  "Интерактивный выбор буфера с учётом EXWM и вкладок.
Если буфер виден — переключаемся; иначе ищем вкладку или открываем."
  (interactive)
  (let* ((buf-name (consult--read (mapcar #'buffer-name (buffer-list))
                                  :prompt "Buffer: "
                                  :require-match t
                                  :category 'buffer))
         (buf (get-buffer buf-name)))
    (if (and (get-buffer-window buf 'visible)
             (with-current-buffer buf (derived-mode-p 'exwm-mode)))
        (select-window (get-buffer-window buf 'visible))
      (if (with-current-buffer buf (derived-mode-p 'exwm-mode))
          (let* ((orig-tab (1+ (tab-bar--current-tab-index)))
                 (tabs (tab-bar-tabs))
                 tab-found)
            (cl-loop for i from 1 to (length tabs) do
                     (unless (= i orig-tab)
                       (tab-bar-select-tab i)
                       (when (get-buffer-window buf 'visible)
                         (setq tab-found t)
                         (cl-return))))
            (unless tab-found
              (tab-bar-select-tab orig-tab))
            (switch-to-buffer buf))
        (switch-to-buffer buf)))))

(defun pro/consult-buffer-or-recentf ()
  "Выбор буфера или recentf-файла в едином списке через Consult-multi."
  (interactive)
  (recentf-mode 1)
  (let* ((buffers (mapcar #'buffer-name (consult--buffer-query :sort 'visibility)))
         (recent (seq-filter (lambda (f) (not (get-buffer f))) recentf-list))
         (sources `((:name "Buffers" :narrow ?b :category buffer :items ,buffers)
                    (:name "Recent Files" :narrow ?r :category file :items ,recent)))
         (result (consult--multi sources
                                 :prompt "Buffer or Recentf: "
                                 :require-match t
                                 :history 'consult--buffer-history
                                 :sort nil)))
    (cond
     ((and (stringp result) (get-buffer result))
      (let ((buf (get-buffer result)))
        (if (and (get-buffer-window buf 'visible)
                 (with-current-buffer buf (derived-mode-p 'exwm-mode)))
            (select-window (get-buffer-window buf 'visible))
          (if (with-current-buffer buf (derived-mode-p 'exwm-mode))
              (let* ((orig-tab (1+ (tab-bar--current-tab-index)))
                     (tabs (tab-bar-tabs))
                     tab-found)
                (cl-loop for i from 1 to (length tabs) do
                         (unless (= i orig-tab)
                           (tab-bar-select-tab i)
                           (when (get-buffer-window buf 'visible)
                             (setq tab-found t)
                             (cl-return))))
                (unless tab-found
                  (tab-bar-select-tab orig-tab))
                (switch-to-buffer buf))
            (switch-to-buffer buf)))))
     ((and (stringp result) (file-exists-p result)) (find-file result))
     ((null result) (message "Ничего не выбрано."))
     ((not (stringp result)) (message "Ошибка: выбор не строка: %S" (type-of result)))
     (t (message "Неизвестный выбор: %S" result)))))

(provide 'про-быстрый-доступ)
;;; про-быстрый-доступ.el ends here
