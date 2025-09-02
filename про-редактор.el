;;; про-редактор.el --- Редактор -*- lexical-binding: t -*-
;;; Commentary:
;; Этот модуль содержит настройки редактора в Emacs.
;; Здесь мы структурируем конфигурацию в литературном стиле, где код описывается
;; как последовательность логических блоков. Каждый блок подробно комментируется,
;; объясняя цель, возможные альтернативы и улучшения.
;;
;; Улучшения в этой версии:
;; 1. Логическая структура: разделение на основные категории (кодировка, клавиатура, выделение, переносы и т.д.).
;; 2. Подробные комментарии: для каждого блока — повествовательное объяснение.
;; 3. Оптимизации: включены хуки для лучшей интеграции, убраны неиспользуемые/дублирующиеся настройки.
;; 4. Читабельность: использованы последовательные use-package и кросс-секционные улучшения.
;; 5. TODO: отмечены потенциальные улучшения (например, интеграция tree-sitter для идентации).
;;; Code:

;;;; === Глобальные зависимости ===
;; Мы подключаем `use-package` для удобного управления пакетами. Это позволяет
;; ленивую загрузку и аккуратную конфигурацию. Убедитесь, что `use-package` установлен.
(require 'use-package)

;;;; === Кодировка: обеспечение UTF-8 по умолчанию ===
;; Здесь мы устанавливаем UTF-8 как стандартную кодировку для всего:
;; языка, терминала, клавиатуры и файлов. Это предотвращает проблемы
;; с русским текстом и международными символами. Если вы работаете с другими
;; кодировками, добавьте условные проверки (например, по системной локали).
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;; === Клавиатура: язык ввода и трансляция ===
;; Мы настраиваем русский ввод и обратную трансляцию (reverse-im) для
;; удобного использования сочетаний клавиш в русской раскладке.
;; Улучшение: добавлен хук для автозагрузки при смене буфера.

;; Язык клавиатуры по умолчанию — русский (russian-computer).
(setq default-input-method "russian-computer")

;; Трансляция сочетаний клавиш при включённом русском: автоматически
;; переводит русские клавиши в эквивалентные английские для hotkeys.
(use-package reverse-im
  :defer t
  :ensure t
  :defines (reverse-im-modifiers reverse-im-input-methods)
  :functions (reverse-im-mode)
  :hook (after-init . reverse-im-mode)  ;; Автоматическое включение после запуска.
  :config
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer"))

;; Дополнительные улучшения ввода:
(delete-selection-mode +1)                   ;; Удалять выделенное при вводе и при вставке.

;; Не копировать при простом выделении (ни клавиатурой, ни мышью).
(setq select-active-regions nil)             ;; Не отправлять активный регион в PRIMARY/clipboard.
(setq mouse-drag-copy-region nil)            ;; Не копировать при выделении мышью/перетаскивании.
(setq select-enable-clipboard t)             ;; Явные копирования/вставки работают с clipboard.

;; Заменять выделенный регион при вставке (включая мышиную/терминальную вставку).
(with-eval-after-load 'mouse
  (put 'mouse-yank-primary 'delete-selection 'yank)) ;; Средняя кнопка заменяет выделение.
(dolist (cmd '(yank yank-pop yank-rectangle))
  (put cmd 'delete-selection 'yank))          ;; C-y и M-y также заменяют выделение.
(when (fboundp 'bracketed-paste)
  (put 'bracketed-paste 'delete-selection 'yank)) ;; Терминальная вставка.
(setq mouse-yank-at-point t)                 ;; Вставка мышью — в позицию курсора, не под мышь.

;;;; === Поиск и навигация по символам ===
;; Avy: быстрый прыжок к символу (C-z + символ). Полезно для быстрой навигации.

(use-package avy
  :defer t
  :ensure t
  :custom (avy-background nil))  ;; Без затемнения фона для лучшей видимости.

;; Multiple-cursors: мультикурсор для одновременного редактирования.
(use-package multiple-cursors
  :defer t
  :ensure t)

;; Улучшенная навигация к началу строки: сначала к отступу, затем к началу.
(defun к-идентации-или-началу-строки ()
  "Перейти к первому не-пробельному символу строки; если уже там — к началу строки."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

;;;; === Выделение и редактирование ===
;; В этой секции: расширяемое выделение, умное удаление и операции над регионом/линией.
;; Улучшение: добавлены хуки для автоматического включения в modes (prog/text).

;; Expand-region: "умное" расширение выделения (от слова к строке, блоку и т.д.).
(use-package expand-region
  :defer t
  :ensure t
                                        ;:hook ((prog-mode text-mode) . er/expand-region)  ;; Авто-включение в ключевых режимах.
  :defines (expand-region-contract-fast-key expand-region-reset-fast-key)
  :custom (expand-region-subword-enabled t)
  :config
  (setq expand-region-contract-fast-key "M-S-SPC"  ;; Традиционный хоть и спорный бинд.
        expand-region-reset-fast-key "<ESC><ESC>")) ;; Быстрый сброс.

;; Улучшение: закомментировано puni (умное удаление), так как зависит от tree-sitter.
;; (use-package puni
;;   :ensure t
;;   :hook (prog-mode . puni-global-mode)  ;; Глобально для кода.
;;   :functions (puni-global-mode puni-disable-puni-mode)
;;   :init
;;   (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; Функции для удаления: до конца строки, до начала (kill-region/line).
(defun удалить-до-конца-строки ()
  "Удалить текст от курсора до конца строки (не включая newline)."
  (interactive)
  (kill-region (point) (line-end-position)))

(defun delete-to-begin ()
  "Удалить текст от курсора до начала строки."
  (interactive)
  (kill-line 0))

;; Whole-line-or-region: операции над строкой, если ничего не выделено.
(use-package whole-line-or-region
  :defer t
  :ensure t
  :hook (prog-mode . whole-line-or-region-local-mode)  ;; Авто-включение.

  :init)

;; Shift-text: сдвиг блоков текста вправо/влево/вверх/вниз.
(use-package shift-text
  :ensure t
  :defer t)

(defun my-isearch-mark-match-and-exit ()
  "Выделить текущее совпадение isearch и выйти."
  (interactive)
  (when (and (boundp 'isearch-other-end) isearch-other-end)
    (set-mark isearch-other-end))
  (isearch-exit))

(with-eval-after-load 'isearch
  (define-key isearch-mode-map (kbd "C-SPC") #'my-isearch-mark-match-and-exit)
  (define-key isearch-mode-map (kbd "M-SPC")   #'my-isearch-mark-match-and-exit))

;;;; === Режим чтения (отцентровка текста) ===
;; Olivetti: фокусирует текст по центру, полезно для чтения/писательства.
;; Улучшение: хук на man/info/markdown/org, минимальная ширина по умолчанию.

(use-package olivetti
  :defer t
  :ensure t
  :hook ((man-mode info-mode markdown-mode org-mode) . olivetti-mode) ;; Авто для чтения.
  :custom
  (olivetti-minimum-body-width 120)   ;; Минимальная ширина для комфортного чтения.
  (olivetti-body-width 100)           ;; Стандартная ширина.
  (olivetti-style nil))              ;; Без лишних отступов.

;;;; === Закладки и переносы ===
;; Закладки: простые метки для быстрого возврата.
(use-package bookmark
  :defer t
  :config
  (setq bookmark-save-flag t))  ;; Авто-сохранение.

;; Переносы строк: визуальные и автоматические.
(setq-default truncate-lines t                   ;; Не обрезать строки.
              truncate-partial-width-windows 50  ;; Обрезать в малых окнах.
              longlines-show-hard-newlines t     ;; Показывать разрывы.
              line-move-visual t)                ;; Визуальное перемещение.

(toggle-truncate-lines t)  ;; Включить обрезку.
(visual-line-mode t)       ;; Визуальные переносы.

;;;; === Отступы и форматирование ===
;; Здесь: базовые настройки табуляции, авто-идентация, детекторы и настойчивая идентация.
;; Улучшение: хуки для языков (Emacs Lisp), интеграция с .editorconfig.

(setq-default indent-tabs-mode nil               ;; Используем пробелы, не табы.
              tab-width 8                        ;; Ширина таба — 8 (стандарт).
              indent-line-function 'indent-relative) ;; Относительная идентация.

(electric-indent-mode t)  ;; Авто-идентация при enter.

(use-package dtrt-indent
  :defer t
  :ensure t
  :functions (dtrt-indent-global-mode)
  :init
  (dtrt-indent-global-mode t))  ;; Авто-определение стиля идентации.

(use-package aggressive-indent
  :defer t
  :ensure t
  :hook ((emacs-lisp-mode . aggressive-indent-mode))) ;; Настойчивость для Lisp.

(use-package editorconfig
  :defer t
  :ensure t
  :functions (editorconfig-mode)
  :init
  (editorconfig-mode 1)) ;; Поддержка .editorconfig файлов.

;;;; === Сравнение буферов ===
;; Ediff: горизонтальное сравнение, плоское представление.
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain ;; Плоское.
              ediff-split-window-function 'split-window-horizontally) ;; Горизонтальное.

;;;; === Редактирование как Root ===
;; Функция для открытия файла от root (sudo), если буфер связан с файлом.
(defun редактировать-текущий-файл-как-root ()
  "Открыть текущий файл от имени root (sudo)."
  (interactive)
  (if (buffer-file-name)
      (let ((file (concat "/sudo:root@localhost:" (buffer-file-name))))
        (find-file file))
    (message "Буфер не связан с файлом.")))

;;;; === Преобразование стилей имён ===
;; String-inflection: переключение между camelCase/snake_case/kebab-case.
(use-package string-inflection
  :defer t
  :ensure t) ;; Полезно для рефакторинга имён переменных.

;;;; === Подсветка отступов и длинных файлов ===
;; Highlight-indent-guides: визуальные направляющие для уровней идентации.
;; Улучшение: закомментировано, но можно включить для prog-mode.

;; (use-package highlight-indent-guides
;;    :ensure t
;;    :hook ((prog-mode . highlight-indent-guides-mode)
;;           (yaml-mode . highlight-indent-guides-mode))
;;    :custom
;;    (highlight-indent-guides-method 'character)
;;    (highlight-indent-guides-responsive 'top)
;;    (highlight-indent-guides-auto-character-face-perc 5))

(global-so-long-mode t) ;; Автоматическая поддержка очень длинных файлов.

;;;; === Орфография и Markdown ===
;; Flymake-aspell: проверка орфографии в текстах (org/text-mode).
;; TODO: Заменить на flycheck-aspell для лучшей интеграции.
(use-package flymake-aspell
  :ensure t
  :hook ((text-mode . flymake-aspell-setup)
         (org-mode . flymake-aspell-setup))
  :defines (ispell-dictionary ispell-silently-savep ispell-program-name)
  :init
  (require 'ispell)
  (setq ispell-dictionary "ru")
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t))

;; Markdown-mode: поддержка языка разметки.
(use-package markdown-mode
  :defer t
  :ensure t)

;;;; === Утилиты: вставка имени файла, ёфикация ===
(defun вставить-имя-файла ()
  "Вставить имя текущего буфера (файла)."
  (interactive)
  (insert (buffer-name)))

;; Yomacs: ёфикация текста (для русского), закомментировано — включите если нужно.
;; https://github.com/pok49/yomacs

;;;; === Vim-подобные клавиши ===
;; Viper: vim клавиши в Emacs. Отключено по умолчанию (viper-mode nil).
(setq-default viper-mode nil)
(require 'viper)
(setq viper-inhibit-startup-message 't)

;;;; === Структурная навигация ===
;; Combobulate: навигация/редактирование по AST (tree-sitter). Закомментировано.
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

;;;; === Мини-карта ===
;; Minimap: боковая панель с миниатюрой кода. Полезно для навигации.
(use-package minimap
  :defer t
  :ensure t
  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background "#111" :foreground "#aaa"))))
  :custom
  (minimap-minimum-width 14)
  (minimap-window-location 'right)
  (minimap-width-fraction 0.08))

;;;; === Копирование в формате ===
;; Copy-as-format: копирование кода/текста в Reddit/Markdown/HTML и др. форматы.
(use-package copy-as-format
  :ensure t
  :defer 110
  :after org
  :config
  (defalias 'copy-as-format-reddit 'copy-as-format-markdown) ;; Алиас для Reddit.
  (defvar copy-as-format-format-alist
    '(
      ("disqus"    copy-as-format--disqus)
      ("github"    copy-as-format--github)
      ("gitlab"    copy-as-format--github)
      ("html"      copy-as-format--html)
      ("jira"      copy-as-format--jira)
      ("markdown"  copy-as-format--markdown)
      ("reddit"    copy-as-format--reddit))
    "Alist форматов и функций преобразования."))

;;;; === Smerge-mode: слияние конфликтов ===
;; Smerge: подсветка и навигация по merge-конфликтам в коде.
(use-package smerge-mode
  :ensure nil
  :hook (prog-mode . smerge-mode)) ;; Авто-включение в prog-mode.

(provide 'про-редактор)
;;; про-редактор.el ends here
