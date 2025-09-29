;;; про-код.el --- Конфигурация среды программирования в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: programming, treesit, eglot, formatting
;; URL:
;;
;;; Commentary:
;;
;; Этот файл настраивает среду программирования, следуя принципам
;; литературного программирования: код представлен как
;; повествование, где каждая секция объясняется и логически связана с
;; остальными. Мы стремимся к ясности, модульности и элегантности,
;; в лучших традициях Emacs — с использованием =use-package= для
;; декларативной конфигурации, хуков для автоматизации и кастомных
;; функций для расширения.
;;
;; Структура файла:
;; 0. Введение и зависимости: Обязательные require и установки.
;; 1. Синтаксический анализ: Tree-sitter для современных языков.
;; 2. Языковой сервер: Eglot для автодополнения и рефакторинга.
;; 3. Скобки и пары: Подсветка, навигация и автозавершение.
;; 4. Идентификаторы и цвета: Подсветка переменных и цветов.
;; 5. Форматирование и проверка: Автоформатирование и linting.
;; 6. Отладка и инструменты: Дебаггер, RE-builder, folding.
;; 7. Специфические языки: Bash, Emmet и другие.
;; 8. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-код) в вашем init.el.
;; Рекомендуется интегрировать с projectile для проектов.
;;
;; Замечания: Мы минимизируем глобальные побочные эффекты, предпочитаем
;; локальные хуки и deferred loading для производительности.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые утилиты. =загрузить= — предположительно
;; кастомная функция для ленивой загрузки. =use-package= — стандарт
;; для модульной конфигурации. =установить-из= — для установки из репозиториев.

(require 'загрузить)
(require 'use-package)
(require 'установить-из)

;;;; 1. Синтаксический анализ с Tree-sitter
;; Tree-sitter предоставляет инкрементальный парсинг для точной
;; подсветки и навигации. Мы активируем его для ключевых языков,
;; ремаппя стандартные режимы на tree-sitter версии.

;;;;; 1.1 Базовая поддержка Tree-sitter
(use-package treesit
  :when (and (fboundp 'treesit-available-p) (treesit-available-p))
  :custom
  (major-mode-remap-alist
   ;; Ремаппинг: Заменяем старые режимы на tree-sitter аналоги для
   ;; лучшей производительности и точности.
   '((c-mode . c-ts-mode)
     (c++-mode . c++-ts-mode)
     (cmake-mode . cmake-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (css-mode . css-ts-mode)
     (js-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (json-mode . json-ts-mode)
     (python-mode . python-ts-mode)
     (sh-mode . bash-ts-mode)
     (typescript-mode . typescript-ts-mode))))
;; Ничего дополнительного не требуется; Emacs берёт на себя.

;;;;; 1.2 Автоматическая установка парсеров
;; =treesit-auto= упрощает: автоматически устанавливает и применяет
;; парсеры для открытых файлов, спрашивая при необходимости.
(use-package treesit-auto
  :ensure t
  :functions (treesit-auto-add-to-auto-mode-alist global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)  ; Запрашивать установку грамматик.
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;;;; 1.3 Индентация на основе Tree-sitter
;; Для точной индентации, основанной на синтаксисе.
(use-package tree-sitter-indent
  :ensure t)

;;;; 2. Языковой сервер с Eglot
;; Eglot интегрирует LSP (Language Server Protocol) для
;; автодополнения, рефакторинга и diagnostics. Мы настраиваем
;; клавиши, хуки и кастомы для удобства.

(require 'jsonrpc)  ; Необходим для Eglot.

(use-package eglot
  :functions (eglot-rename eglot-code-actions)
  :bind (:map eglot-mode-map
              ("C-c r" . eglot-rename)
              ("M-'" . eglot-inlay-hints-mode)
              ("C-<down-mouse-1>" . xref-find-definitions)
              ("M-." . xref-find-definitions)
              ("C-M-." . xref-find-references)
              ("C-S-<down-mouse-1>" . xref-find-references)
              ("C-c ." . eglot-code-actions))
  :hook
  (bash-ts-mode . eglot-ensure)  ; Авто-включение для Bash.
  :custom
  (eglot-autoshutdown t)         ; Авто-выключение сервера.
  (eglot-sync-connect 3)         ; Таймауты для стабильности.
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size '(:size 2000 :format full))
  (eglot-send-changes-idle-time 0)
  :config
  ;; Кастомная конфигурация для Haskell (пример).
  (setq-default eglot-workspace-configuration
                '((haskell (plugin (stan (globalOn . :json-false))))))
  ;; Явная привязка HLS для haskell-mode (до запуска Eglot).
  (add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  ;; Отключаем логирование для производительности.
  (fset #'jsonrpc--log-event #'ignore))

;;;;; 2.1 Подсказки поверх кода
;; Дополнения к Eglot: оверлеи, hierarchies и sideline.
(use-package coverlay
  :ensure t)

;; (use-package sideline-eglot
;;   :ensure t
;;   :init (setq sideline-backends-right '(sideline-eglot)))

(use-package eglot-hierarchy
  :init (установить-из :repo "dolmens/eglot-hierarchy"))

;; Поддержка Nix в Eglot: для языков, где сервер из Nix, проверка и запуск через nix-shell.
(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (when (and (file-exists-p "shell.nix") (executable-find "nix-shell"))
              (setq-local eglot-server-programs
                          (append eglot-server-programs
                                  '((prog-mode . ("nix-shell" "--run" "default-lsp-server"))))))))

;;;; 3. Скобки и пары
;; Раздел для работы со скобками: подсветка, навигация, автопары
;; и выделение глубины. Это улучшает читаемость сложного кода.

;;;;; 3.1 Подсветка скобок
(use-package paren-face
  :ensure t
  :if window-system
  :functions (global-paren-face-mode)
  :custom
  (paren-face-regexp "[][(){}]")
  :config (global-paren-face-mode t))

;;;;; 3.2 Навигация по парным скобкам
;; Кастомная функция для прыжков между скобками.
(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis if on one, else forward sexp.
With prefix ARG, do it that many times; negative for backward."
  (interactive "^p")
  (cond
   ((looking-at "\\s(") (forward-sexp arg))
   ((looking-back "\\s)" 1) (backward-sexp arg))
   ((looking-at "\\s)") (forward-char) (backward-sexp arg))
   ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;;;; 3.3 Автоматические пары
;; Electric-pair для автозавершения скобок.
;; TODO: Рассмотреть переход на tree-sitter-based альтернативу.
(use-package electric-pair
  :hook
  (after-init . electric-pair-mode)
  (emacs-lisp-mode . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;;;;; 3.4 Подсветка глубины скобок
;; Цветовая дифференциация по уровням вложенности.
(defvar my/hl-paren-face)
(setq my/hl-paren-face (face-foreground 'default))

(use-package highlight-parentheses
  :ensure t
  :defines (global-highlight-parentheses-mode)
  :custom
  (hl-paren-colors
   '("#8e44ad" "#3498db" "#1abc9c" "#27ae60" "#f1c40f" "#f39c12" ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face
     ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face))
  ;; (hl-paren-background-colors '(nil nil nil nil nil))
  :config (global-highlight-parentheses-mode t)
  :init
  (add-hook 'after-load-theme-hook
            (lambda ()
              (global-highlight-parentheses-mode -1)
              (sleep-for 1)
              (global-highlight-parentheses-mode t))))

;;;;; 3.5 Подсветка глубины индентации
;; Визуальные гайды для отступов в коде.
(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (css-base-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . highlight-indent-guides-mode))

;;;; 4. Идентификаторы и цвета
;; Подсветка переменных, символов и цветов для лучшей читаемости.

;;;;; 4.1 Цветовая подсветка идентификаторов
(use-package color-identifiers-mode
  :defer t
  :if window-system
  :defines (color-identifiers:modes-alist color-identifiers:re-not-inside-class-access)
  :ensure t
  :custom
  (color-identifiers-coloring-method 'hash)
  (color-identifiers:num-colors 16)
  (color-identifiers:color-luminance 0.3)
  (color-identifiers:min-color-saturation 0.2)
  (color-identifiers:max-color-saturation 0.7)
  :config
  (add-to-list 'color-identifiers:modes-alist
               '(js-ts-mode "" "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                            (nil font-lock-variable-name-face tree-sitter-hl-face:variable))))

;;;;; 4.2 Альтернативная подсветка идентификаторов
(use-package rainbow-identifiers
  :defer t
  :if window-system
  :ensure t)

;;;;; 4.3 Оверлей для символов
(use-package symbol-overlay
  :ensure t)

;;;;; 4.4 Подсветка цветов в коде
(use-package rainbow-mode
  :defer t
  :ensure t
  :hook (prog-mode . rainbow-mode))

;;;; 5. Форматирование и проверка
;; Автоматическое форматирование и статическая проверка кода.

;;;;; 5.1 Универсальное форматирование
(use-package format-all
  :defer t
  :ensure t
  :hook
  (ess-r-mode . format-all-mode)
  (python-mode . format-all-mode)
  ;; (emacs-lisp-mode . format-all-mode)  ; Раскомментировать при необходимости.
  (format-all-mode . format-all-ensure-formatter)
  :config
  (custom-set-variables
   '(format-all-formatters '(("Python" black) ("R" styler) ("Haskell" ormolu)))))

;;;;; 5.2 Альтернативный форматтер
(use-package apheleia
  :defer t
  :ensure t
  :hook
  (js-ts-mode . apheleia-mode)
  (typescript-ts-mode . apheleia-mode)
  :config)

;;;;; 5.3 Статическая проверка (Flymake)
(use-package flymake
  :ensure t
  :custom
  (flymake-no-changes-timeout 0.01)
  (elisp-flymake-byte-compile-load-path load-path)
  :hook (emacs-lisp-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-]" . flymake-goto-next-error)
              ("M-[" . flymake-goto-prev-error)
              ("M-\\" . flymake-show-buffer-diagnostics)))

;;;; 6. Отладка и инструменты
;; Дебаггер, конструктор regex и свёртка кода.

;;;;; 6.1 Дебаггер (Dape)
(require 'projectile)

(use-package dape
  :defer t
  :ensure t
  :defines (dape-buffer-window-arrangement dape-cwd-fn)
  :config
  (setq dape-buffer-window-arrangement 'gud)
  (setq dape-key-prefix "C-.")
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t)))
  (setq dape-cwd-fn (lambda (&optional skip-tramp-trim)
                      (let ((root (projectile-project-root)))
                        (if (and (not skip-tramp-trim) (tramp-tramp-file-p root))
                            (tramp-file-name-localname (tramp-dissect-file-name root))
                          root)))))

;;;;; 6.2 Конструктор регулярных выражений
;; RE-builder для визуального создания regex.
(загрузить 're-builder)
(setq reb-re-syntax 'read)  ; Синтаксис string для меньшего экранирования.

;;;;; 6.3 Свёртка кода
(use-package treesit-fold
  :defer t
  :init (установить-из :repo "abougouffa/treesit-fold"))

;; (use-package treesit-fold-indicators
;;   :init (установить-из :repo "emacs-tree-sitter/treesit-fold"))

(use-package origami
  :defer t
  :ensure t
  :bind (:map origami-mode-map
              ("C-<tab>" . origami-recursively-toggle-node)
              ("C-TAB" . origami-recursively-toggle-node)
              ("C-S-<tab>" . origami-recursively-toggle-node)
              ("C-M-<tab>" . origami-toggle-all-nodes)))

;;;; 7. Специфические языки и инструменты
;; Emmet для web, Bash и заголовки функций.

;;;;; 7.1 Zen-кодинг (Emmet)
(use-package emmet-mode
  :ensure t
  :hook ((web-mode tsx-ts-mode typescript-ts-mode) . emmet-mode)
  :defines (emmet-indent-after-insert emmet-indentation
                                      emmet-expand-jsx-className? emmet-move-cursor-between-quotes
                                      emmet-self-closing-tag-style emmet-jsx-major-modes)
  :config
  (setq emmet-indent-after-insert nil
        emmet-indentation 2
        emmet-expand-jsx-className? t
        emmet-move-cursor-between-quotes t
        emmet-self-closing-tag-style " /")
  (add-to-list 'emmet-jsx-major-modes 'tsx-ts-mode))

;;;;; 7.2 Bash-поддержка
;; Flymake для shellcheck в Bash.
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :hook ((bash-ts-mode sh-mode) . flymake-shellcheck-load)
  :init)

;;;;; 7.3 Заголовки функций (Topsy)
;; Показывает текущую функцию в header-line при скролле.
(use-package topsy
  :ensure t
  :hook
  (prog-mode . topsy-mode)
  (magit-section-mode . topsy-mode)
  :init
  (defface topsy-header-line
    '((t :inherit default :foreground "gray40"))
    "Face for the =topsy-mode' header line.")
  (defconst topsy-header-line-format
    '(:eval (list (propertize " " 'display '((space :align-to 0)))
                  (propertize (or (funcall topsy-fn) "") 'face 'topsy-header-line)))
    "The header line format used by =topsy-mode'."))

;; Перевод комментариев

(use-package cct-mode
  :load-path "~/Code/cct-mode/")

(provide 'про-код)
;;; про-код.el ends here
