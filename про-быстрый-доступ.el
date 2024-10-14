;;; про-быстрый-доступ.el --- Быстрый поиск имён в вертикальных списках -*- lexical-binding: t -*-
;;; Commentary:
;; Настройка инструментов для быстрого доступа и поиска в Emacs.

;;; Code:

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
   (consult-line-start-from-top t) ;; Начинать поиск сверху
   (xref-show-definitions-function #'consult-xref) ;; Отображение определений
   (xref-show-xrefs-function #'consult-xref)) ;; Отображение ссылок
  :config
  (autoload 'projectile-project-root "projectile")
  (require 'consult-xref)
  (setq consult-project-root-function #'projectile-project-root)) ;; Установить функцию корня проекта

;;;; Дополнение сниппетов

(use-package consult-yasnippet
  :ensure t) ;; Убедиться в наличии пакета

;;;; Поиск по документации (dash)

(use-package consult-dash
  :ensure t
  :defines (consult-dash)
  :config 
  (consult-customize consult-dash
                     :initial (thing-at-point 'symbol))) ;; Начальное значение для поиска по символу

;;;; Поиск файлов

(require 'dired) ;; Подключить модуль dired

(use-package consult-ag
  :ensure t
  :after dired
  :functions (consult-ag)
  :config
  (defun искать-по-файлам-отсюда ()
    "Поиск по файлам от текущего пути." ;; Функция поиска
    (interactive) 
    (consult-ag default-directory)) ;; Поиск в текущем каталоге

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

(use-package consult-lsp
  :after (lsp consult) 
  :ensure t 
  :disabled t) 

(use-package consult-eglot
  :after (eglot consult)
  :defines (eglot-mode-map)
  :functions (consult-eglot-symbols) ;; Функция для поиска символов
  :ensure t ;; Убедитесь, что пакет будет установлен
  :bind (:map eglot-mode-map ("C-c C-." . #'consult-eglot-symbols))) ;; Привязка для поиска символов

(provide 'про-быстрый-доступ) ;; Экспортируем текущий модуль
;;; про-быстрый-доступ.el ends here
