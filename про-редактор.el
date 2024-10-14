;;; про-редактор.el --- Редактор -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Кодировка

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
    default-file-name-coding-system 'utf-8
    x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;; Клавиатура

;; Язык клавиатуры

(setq default-input-method "russian-computer")

;; Трансляция сочетаний клавиш при включеном русском

(use-package reverse-im
  :ensure t
  :defines (reverse-im-modifiers reverse-im-input-methods)
  :functions (reverse-im-mode)
  :config
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

;; Удалять выделенное при печати нового

(delete-selection-mode +1)

;; Средняя кнопка мыши вставляет в позицию курсора

(setq mouse-yank-at-point t)

;; Можно прыгнуть сразу на любой символ, нажав C-z и этот символ

(use-package avy
  :ensure t
  :defer t
  :custom ((avy-background nil)))

;; Мульти-курсор

(use-package multiple-cursors
  :ensure t)

;; При перемещении в начало строки *<C-a>*, сперва прыгать к  идентации, затем - к началу строки

(defun к-идентации-или-началу-строки ()
  "вернуться к идентации или началу строки."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

;; Подсвечивать курсор при мгновенном перемещении, чтобы он не потерялся

;; (use-package beacon
;;   :custom
;;   (beacon-color "#ffaa00")
;;   :hook (after-init . beacon-mode))

;;;; Выделение

;; http://www.cs.man.ac.uk/~chl/secondary-selection.html
;; https://emacs.stackexchange.com/questions/17056/what-is-the-origin-of-the-term-yank

;; Выделять "изнутри"

(use-package expand-region
  :ensure t
  :defines (expand-region-contract-fast-key expand-region-reset-fast-key)
  :custom (expand-region-subword-enabled  t)
  :config
  (setq expand-region-contract-fast-key "M-S-SPC"
       expand-region-reset-fast-key "<ESC><ESC>"))

;; Умное удаление

;; (use-package puni
;;   :ensure t
;;   :functions (puni-global-mode puni-disable-puni-mode)
;;   :init
;;   (puni-global-mode t)
;;   (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; Удалить до конца строки, но не CR

(defun удалить-до-конца-строки ()
  "Удалить до конца строки."
  (interactive)
  (kill-region (point) (line-end-position)))

;; Операция над выделением или текущей линией

(use-package whole-line-or-region
  :ensure t
  :init)

;; Функция delete-to-begin, для вырезания от курсора до начала строки:

(defun delete-to-begin ()
  "Удалить до начала строки."
  (interactive)
  (kill-line 0))

;;;; Перемещение блоков

(use-package shift-text
  :ensure t
  :defer t)

;;;; Отображения текста по центру ("режим чтения")

(use-package olivetti
  :ensure t
  :hook ((text-mode Man-mode Info-mode) . olivetti-mode)
  :custom ((olivetti-minimum-body-width 80)
          (olivetti-body-width 80)))

;;;; Закладки

(use-package bookmark
  :config
  (setq bookmark-save-flag t))


;;;; Переносы

(setq-default truncate-lines t
         truncate-partial-width-windows 50
         longlines-show-hard-newlines t
         line-move-visual t)

(toggle-truncate-lines t)
(visual-line-mode t)

;;;; Конфигурация отступов

;; По умолчанию отступы в 2 пробела

(setq-default indent-tabs-mode nil
           tab-width 4
           indent-line-function 'indent-relative
           default-tabs-width 4)

;; Автоматически выравнивать при переводе строки

(electric-indent-mode t)

;; Автоматически определять отступы

(use-package dtrt-indent
  :ensure t
  :functions (dtrt-indent-global-mode)
  :init
  (dtrt-indent-global-mode t))

;; Настойчиво делать идентацию для некоторых типов файлов

(use-package aggressive-indent
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)))

;; Настройка отступов берётся из файла .editorconfig

(use-package editorconfig
  :ensure t
  :functions (editorconfig-mode)
  :init
  (editorconfig-mode 1))

;;;; Сравнение

;; Плоское, горизонтальное расположение при сравнении буферов

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain
           ediff-split-window-function 'split-window-horizontally)

;;;; Редактировать как Root

(defun редактировать-текущий-файл-как-root ()
  "Редактировать как root файл, связанный с текущим буфером."
  (interactive)
  (if (buffer-file-name)
      (let ((file (concat "/sudo:root@localhost:" (buffer-file-name))))
        (find-file file))
    (message "Буфер не связан с файлом.")))

;;;; Переключение CamelCase/snakeCase/dash-divided итд

(use-package string-inflection
  :ensure t)

;;;; Подсветка идентации

;; (use-package highlight-indent-guides
;;    :ensure t
;;    :hook ((prog-mode . highlight-indent-guides-mode)
;;           (yaml-mode . highlight-indent-guides-mode))
;;    :custom
;;    (highlight-indent-guides-method 'character)
;;    (highlight-indent-guides-responsive 'top)
;;    (highlight-indent-guides-auto-character-face-perc 5))

;;;; Поддержка очень длинных файлов

(global-so-long-mode t)

;;;; Проверка орфографии

;; TODO: Заменить на flycheck-aspell

;; (use-package flymake-aspell
;;   :ensure t
;;   :hook ((text-mode . flymake-aspell-setup)
;;        (org-mode . flymake-aspell-setup))
;;   :init
;;   (require 'ispell)
;;   (setq ispell-dictionary "ru")
;;   (setq ispell-program-name "aspell")
;;   (setq ispell-silently-savep t))

;;;; Поддержка языка  разметки Markdown

(use-package markdown-mode :ensure t)

;;;; Вставить имя файла

(defun вставить-имя-файла ()
  "Вставить имя файла."
  (interactive)
  (insert (buffer-name)))

;;;; Ёфикация

;; https://github.com/pok49/yomacs

;;;; Vim-клавиши

(setq-default viper-mode nil)
(require 'viper)

(setq viper-inhibit-startup-message 't)

;;;; Навигация и структурное редактирование

;; (use-package combobulate
;;   :init (установить-из :repo "mickeynp/combobulate")
;;   :preface
;;   (setq combobulate-key-prefix "M-o")

;;   :hook
;;   ((python-ts-mode . combobulate-mode)
;;    (js-ts-mode . combobulate-mode)
;;    (html-ts-mode . combobulate-mode)
;;    (css-ts-mode . combobulate-mode)
;;    (yaml-ts-mode . combobulate-mode)
;;    (typescript-ts-mode . combobulate-mode)
;;    (json-ts-mode . combobulate-mode)
;;    (tsx-ts-mode . combobulate-mode)))

;;;; Мини-карта

(use-package minimap
  :ensure t
  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background "#111" :foreground "#aaa"))))
  :custom
  (minimap-minimum-width 14)
  (minimap-window-location 'right)
  (minimap-width-fraction 0.08))

(provide 'про-редактор)
;;; про-редактор.el ends here
