;;; про-быстрый-доступ.el --- Быстрый поиск имён в вертикальных списках -*- lexical-binding: t -*-
;;; Commentary:
;; Настройка инструментов для быстрого доступа и поиска в Emacs.

;;; Code:

(require 'projectile)

;;;; Поиск по файлу

;; TODO: Проверить маппинг клавиш на конфликты и оптимизировать.
;; (use-package ctrlf
;;   :ensure t
;;   :defines (ctrlf-default-search-style ctrlf-alternate-search-style)
;;   :functions (ctrlf-mode)
;;   :config
;;   (setq ctrlf-default-search-style 'fuzzy-regexp) ;; Стиль поиска по умолчанию
;;   (setq ctrlf-alternate-search-style 'literal) ;; Альтернативный стиль поиска
;;   (ctrlf-mode -1)) ;; Отключить режим ctrlf по умолчанию

;;;; Вертикальные списки

(use-package vertico
  :ensure t
  :defines (vertico-map)
  :functions (vertico-mode)
  :custom
  (read-file-name-completion-ignore-case t) ;; Игнорировать регистр для имен файлов
  (read-buffer-completion-ignore-case t) ;; Игнорировать регистр для буферов
  (vertico-cycle t) ;; Включить циклический режим
  (completion-ignore-case t) ;; Игнорировать регистр при автодополнении
  :bind (:map vertico-map
                ("s-<tab>" . vertico-next) ;; Переход к следующему элементу
                ("s-<iso-lefttab>" . vertico-previous)) ;; Переход к предыдущему элементу
  :init
  (require 'vertico)
  :config
  (vertico-mode t)) ;; Включить режим vertico

;; Поддержка директорий

;; (use-package vertico-directory
;;   :ensure t
;;   :after (vertico)
;;   :bind
;;   (:map vertico-map
;;           ("RET" . vertico-directory-enter)
;;           ("DEL" . vertico-directory-delete-char))
;;   :config
;;   (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy))

;; В виде таблицы

;; (use-package vertico-grid
;;   :ensure t
;;   :after (vertico))

;;;; Сортировка

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic) ;; Стили автодополнения
       completion-category-defaults nil
       completion-category-overrides
       '((file
          (styles partial-completion))))) ;; Настройки для работы с файлами

;;;; Подписи и дополнительная информация

(use-package marginalia
  :ensure t
  :functions (marginalia-mode)
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)) ;; Настройки аннотаций
  :config
  (marginalia-mode)) ;; Включить режим marginalia

;;;; Функции на базе автодополнения

(use-package consult
  :ensure t
  :defines (consult-project-root-function)
  :functions (consult-customize consult-xref)
  :bind
  (([remap bookmark-jump] . consult-bookmark) ;; Переход к закладкам
   ([remap goto-line] . consult-goto-line) ;; Переход к строке
   ([remap imenu] . consult-imenu) ;; Работа с меню
   ([remap project-switch-to-buffer] . consult-project-buffer) ;; Переключение между буферами проекта
   ([remap repeat-complex-command] . consult-complex-command) ;; Выполнение сложных команд
   ([remap switch-to-buffer] . consult-buffer) ;; Переключение между буферами
   ([remap yank-pop] . consult-yank-pop) ;; Работа с буфером обмена
   :map consult-narrow-map
   ("C-h" . consult-narrow-help) ;; Помощь по сужению
   :map goto-map
   ("m" . consult-mark) ;; Переход к меткам
   ("o" . consult-outline) ;; Работа со структурой документа
   :map help-map
   ("M" . consult-minor-mode-menu)) ;; Доступ к меню минорных режимов
  :custom
  ((consult-preview-key "M-.") ;; Клавиша предварительного просмотра
;;;; 
   (consult-line-start-from-top t) ;; Начинать поиск сверху
   (xref-show-definitions-function #'consult-xref) ;; Отображение определений
   (xref-show-xrefs-function #'consult-xref)) ;; Отображение ссылок
  :config
  (autoload 'projectile-project-root "projectile")
  (require 'consult-xref)
  (setq consult-project-root-function #'projectile-project-root)) ;; Установить функцию корня проекта

;;;; Дополнение сниппетов

(use-package consult-yasnippet
  :ensure t)

;;;; Поиск по документации (dash)

(use-package consult-dash
  :ensure t
  :defines (consult-dash)
  :config
  (consult-customize consult-dash
                     :initial (thing-at-point 'symbol))) ;; Начальное значение для поиска по символу

;;;; Поиск файлов

(require 'dired) ;; Подключить модуль dired

(defun искать-по-файлам-отсюда ()
  "Поиск по файлам от текущего пути." ;; Функция поиска
  (interactive)
  (consult-grep default-directory)) ;; Поиск в текущем каталоге

(use-package consult-ag
  :ensure t
  :after dired
  :functions (consult-ag)
  :config

  ;; Получение маркера в буфере по указанным координатам.
  (defun consult--position-marker (buffer line column)
    "Get marker in BUFFER from LINE and COLUMN."
    (when (buffer-live-p buffer) ;; Проверка существования буфера
      (with-current-buffer buffer
        (save-restriction
          (save-excursion
            (widen) ;; Снятие ограничений
            (goto-char (point-min)) ;; Переход к началу буфера
            (ignore-errors
              (forward-line (- line 1)) ;; Переход на нужную строку
              (forward-char column)) ;; Переход на нужный столбец
            (point-marker)))))) ;; Возвращение маркера текущей позиции

  ;; Привязка клавиши для поиска по файлам
  :bind (:map dired-mode-map
                ("s" . искать-по-файлам-отсюда))) ;; Обработчик для функции поиска

;;;; Поиск по языковому серверу

;; (use-package consult-lsp
;;   :defer t
;;   :after (lsp consult)
;;   :ensure t
;;   :disabled t)


(use-package consult-eglot
  :after (eglot consult)
  :defines (eglot-mode-map)
  :functions (consult-eglot-symbols) ;; Функция для поиска символов
  :ensure t ;; Убедитесь, что пакет будет установлен
  :bind (:map eglot-mode-map ("C-c C-." . #'consult-eglot-symbols))) ;; Привязка для поиска символов


;; Функция для более удобного переключения по буферам, с учётом вкладок.

(require 'cl-lib)
(require 'consult)
(require 'recentf)
(require 'seq)

(defun pro/consult-buffer ()
  "Интерактивный выбор буфера с учётом exwm-режима.
Если буфер уже где-либо отображён, то просто переключаемся на окно с ним.
Если же буфер не виден, то для буферов в exwm-mode ищем вкладку,
на которой он может быть показан, а если не находим — открываем его
в текущем окне. Для остальных буферов просто переключаемся на них."
  (interactive)
  (let* ((buf-name (consult--read (mapcar #'buffer-name (buffer-list))
                                  :prompt "Buffer: "
                                  :require-match t
                                  :category 'buffer))
         (buf (get-buffer buf-name)))
    ;; Если буфер уже отображается где-либо и это Xorg (exwm-mode) окно — выбираем то окно.
    (if (and (get-buffer-window buf 'visible)
             (with-current-buffer buf (derived-mode-p 'exwm-mode)))
        (select-window (get-buffer-window buf 'visible))
      (if (with-current-buffer buf (derived-mode-p 'exwm-mode))
          (let* ((orig-tab (+ 1 (tab-bar--current-tab-index)))
                 (tabs (tab-bar-tabs))
                 tab-found)
            ;; Перебираем вкладки, пытаясь найти ту, где буфер виден.
            (cl-loop for i from 1 to (length tabs) do
                     (unless (equal i orig-tab)
                       (tab-bar-select-tab i)
                       (when (get-buffer-window buf 'visible)
                         (setq tab-found t)
                         (cl-return))))
            ;; Если не нашли, возвращаем исходную вкладку.
            (unless tab-found
              (tab-bar-select-tab orig-tab))
            (switch-to-buffer buf))
        ;; Для всех остальных буферов просто открываем его в текущем окне.
        (switch-to-buffer buf)))))

(defun pro/consult-buffer-or-recentf ()
  "Показывает буферы и recentf-файлы в одном списке.
- Буферы идут первыми, затем — недавно открытые файлы (список recentf).
- Если выбран буфер — стандартное переключение.
- Если выбран недавний файл — открывает его."
  (interactive)
  (recentf-mode 1)
  (let* ((buffers (mapcar #'buffer-name (consult--buffer-query :sort 'visibility)))
         (recent (seq-filter
                  (lambda (f)
                    (not (get-buffer f)))    ;; исключить уже открытые буферы
                  recentf-list))
         (sources
          `(( :name "Buffers"
               :narrow ?b
               :category buffer
               :items ,buffers)
            ( :name "Recent Files"
               :narrow ?r
               :category file
               :items ,recent)))
         (result (consult--multi sources
                                 :prompt "Buffer or Recentf: "
                                 :require-match t
                                 :history 'consult--buffer-history
                                 :sort nil)))
    (cond
     ;; Если буфер с таким именем есть — работаем с ним как с буфером
     ((and (stringp result) (get-buffer result))
      (let ((buf (get-buffer result)))
        (if (and (get-buffer-window buf 'visible)
                 (with-current-buffer buf (derived-mode-p 'exwm-mode)))
            (select-window (get-buffer-window buf 'visible))
          (if (with-current-buffer buf (derived-mode-p 'exwm-mode))
              (let* ((orig-tab (+ 1 (tab-bar--current-tab-index)))
                     (tabs (tab-bar-tabs))
                     tab-found)
                (cl-loop for i from 1 to (length tabs) do
                         (unless (equal i orig-tab)
                           (tab-bar-select-tab i)
                           (when (get-buffer-window buf 'visible)
                             (setq tab-found t)
                             (cl-return))))
                ;; Если не нашли, возвращаем исходную вкладку.
                (unless tab-found
                  (tab-bar-select-tab orig-tab))
                (switch-to-buffer buf))
            (switch-to-buffer buf)))))
     ;; Если строка — возможно путь к файлу
     ((and (stringp result) (file-exists-p result))
      (find-file result))
     ;; Если nil ― ничего не выбрано
     ((null result)
      (message "Ничего не выбрано."))
     ;; Если тип результата не строка — возможно, consult--multi вернул структуру (ошибка или экспериментальный кейс)
     ((not (stringp result))
      (message "Выбор не является допустимой строкой, возможно, ошибка источника: %S" (type-of result)))
     ;; Неизвестная ситуация — оставлено для отладки
     (t
      (message "Неизвестный выбор: %S" result)))))

(provide 'про-быстрый-доступ) ;; Экспортируем текущий модуль
;;; про-быстрый-доступ.el ends here.
