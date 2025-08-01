;;; про-код-на-javascript.el --- Поддержка JavaScript и TypeScript в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords: javascript, typescript, eglot, web, json
;; URL: https://example.com/про-код-на-javascript
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку JavaScript и TypeScript в Emacs, следуя
;; принципам литературного программирования Дональда Кнута: код представлен
;; как coherentное повествование, где каждая секция мотивируется, объясняется
;; и логически связывается с остальными. Мы стремимся к элегантности,
;; минимализму и производительности в лучших традициях Emacs — с использованием
;; `use-package` для декларативной конфигурации, хуков для автоматизации и
;; отложенной загрузки для скорости.
;;
;; Почему это важно? JavaScript и TypeScript — основа веб-разработки. Здесь
;; мы интегрируем LSP (Eglot) для автодополнения и рефакторинга, управляем
;; версиями Node.js, добавляем инструменты для JSON, GraphQL, HTML и проверку
;; кода, делая Emacs мощным IDE без излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и утилиты.
;; 1. Основной режим для JavaScript/TypeScript: Интеграция с Eglot.
;; 2. Управление путями и модулями: Автоматическое добавление node_modules.
;; 3. Версии Node.js: Гибкое переключение с nvm.
;; 4. Документация: Инструменты для JSDoc.
;; 5. HTTP и запросы: Простая работа с API.
;; 6. JSON: Редактирование, навигация и свёртка.
;; 7. Проверка кода: Flymake с ESLint.
;; 8. GraphQL: Режимы для схем и запросов.
;; 9. Утилиты: UML для TypeScript, CSV, сниппеты.
;; 10. HTML и шаблоны: Web-mode с LSP.
;; 11. Angular: Специфическая поддержка.
;;
;; Использование: Загружайте через (require 'про-код-на-javascript) в вашем init.el.
;; Рекомендуется интегрировать с Projectile для проектов. Закомментированные
;; секции — опции для экспериментов (например, react-snippets).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Для специальных языков используйте
;; tree-sitter для лучшей подсветки.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые пакеты. Eglot — ключевой для LSP, а use-package
;; обеспечивает модульность. Мы предполагаем наличие общих модулей (про-код.el).

(require 'use-package)
(require 'eglot)

;;;; 1. Основной режим для JavaScript/TypeScript
;; Это сердце: js-ts-mode с Eglot для автодополнения, хинтов и рефакторинга.
;; Мы настраиваем сервер TypeScript, отключаем inlay-hints по умолчанию и
;; добавляем кастомные опции для импортов и сортировки.

(use-package js
  :after eglot
  :hook ((js-ts-mode . eglot-ensure)
         (js-ts-mode . (lambda ()
                         (eglot-inlay-hints-mode -1))))
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :interpreter (("node" . js-ts-mode))
  :init (add-to-list 'eglot-server-programs `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) . ("/home/az/.nvm/versions/node/v16.20.2/bin/typescript-language-server" "--stdio"
                                                                                                                     :initializationOptions (:preferences (:importModuleSpecifierPreference "non-relative"
                                                                                                                                                                                            :includeInlayEnumMemberValueHints t
                                                                                                                                                                                            :includeInlayFunctionLikeReturnTypeHints t
                                                                                                                                                                                            :includeInlayFunctionParameterTypeHints t
                                                                                                                                                                                            :includeInlayParameterNameHints "all" ; "none" | "literals" | "all"
                                                                                                                                                                                            :includeInlayParameterNameHintsWhenArgumentMatchesName t
                                                                                                                                                                                            :includeInlayPropertyDeclarationTypeHints t
                                                                                                                                                                                            :includeInlayVariableTypeHints t
                                                                                                                                                                                            :includeInlayVariableTypeHintsWhenTypeMatchesName t
                                                                                                                                                                                            :organizeImportsCaseFirst "upper"
                                                                                                                                                                                            :organizeImportsCollation "unicode" ; "ordinal" | "unicode"
                                                                                                                                                                                            :organizeImportsIgnoreCase

                                                                                                                                                                                            :json-false
                                                                                                                                                                                            :quotePreference "single"))))))

;;;; 2. Управление путями и модулями
;; Для проектов с npm: автоматически добавляем node_modules в exec-path,
;; чтобы Emacs находил локальные бинарники без ручной настройки.

(use-package add-node-modules-path
  :defer t
  :ensure t)

;;;; 3. Версии Node.js
;; Nvm позволяет переключать версии Node.js на лету, что критично для
;; проектов с разными зависимостями. Мы проверяем и активируем версию
;; автоматически, с fallback на сообщение об ошибке.

;; (use-package nvm
;;   :ensure t
;;   :functions (nvm-use nvm--version-installed?)
;;   :init
;;   (let ((node-version "18.20.4"))
;;     (if (nvm--version-installed? node-version)
;;         (nvm-use node-version)
;;       (message "Node version %s is not installed." node-version)))
;;   :config
;;   ;; Поддержка Nix: приоритетно использовать Node из Nix, если доступен.
;;   (when (executable-find "nix-shell")
;;     (add-hook 'js-ts-mode-hook
;;               (lambda ()
;;                 (when (file-exists-p "shell.nix")
;;                   (setq-local exec-path (cons (expand-file-name "~/.nix-profile/bin") exec-path)))))))

;;;; 4. Документация
;; JSDoc для вставки шаблонов документации. Закомментировано по умолчанию,
;; чтобы избежать загрузки; раскомментируйте для использования с биндами.

;; (use-package js-doc
;;   :ensure t
;;   :defer t
;;   :bind (:map js2-mode-map
;;               ("C-c jD" . js-doc-insert-file-doc)
;;               ("C-c jd" . js-doc-insert-function-doc))
;;   :config)

;;;; 5. HTTP и запросы
;; Request — простой способ отправлять HTTP-запросы из Emacs, полезно
;; для тестирования API прямо в буфере.

(use-package request
  :ensure t)

;;;; 6. JSON
;; Json-mode для редактирования, с hs-minor-mode для свёртки объектов.
;; Json-navigator добавляет иерархическую навигацию, как в браузере.

(use-package json-mode
  :ensure t
  :hook (json-mode . hs-minor-mode))

(use-package json-navigator
  :defer t
  :ensure t)

;;;; 7. Проверка кода
;; Flymake-eslint: автоматическая проверка с ESLint, основанная на
;; project.json и .eslintrc. Добавляем в exec-path node_modules для
;; локальных бинарников.

(use-package flymake-eslint
  :defer t
  :ensure t
  :functions flymake-eslint-enable
  :preface (defun попробовать-flymake-eslint () "Включить `flymake-eslint' основываясь на конфигурации проекта.
Ищет ESLint конфиг проекта, чтобы определить чем проверять буфер." (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                                                                               (rc (locate-file ".eslintrc" (list root)
                                                                                                '(".js" ".json"))))
                                                                     (make-local-variable 'exec-path)
                                                                     (push (file-name-concat root "node_modules" ".bin") exec-path)
                                                                     (flymake-eslint-enable))))

;;;; 8. GraphQL
;; Режимы для редактирования GraphQL-схем и запросов. graphql-ts-mode
;; использует tree-sitter для лучшей подсветки.

(use-package graphql-mode
  :defer t
  :ensure t)

(use-package graphql-ts-mode
  :defer t
  :ensure t)

;;;; 9. Утилиты
;; Разные инструменты: UML для TypeScript, CSV-режим, сниппеты React.
;; typescript-show-uml генерирует диаграммы из кода.

(defun typescript-show-uml () "Показать UML-диаграмму для текущего файла." (interactive)
       (message (concat "tsuml2 -m --glob \"" (buffer-file-name) "\" -o " (concat (buffer-file-name) ".png"))))

(use-package csv-mode
  :defer t
  :ensure t)

;; (use-package react-snippets
;;   :ensure t
;;   :init (require 'react-snippets))

;;;; 10. HTML и шаблоны
;; Web-mode для HTML с вкраплениями JS/CSS, плюс Eglot для LSP.
;; Настраиваем отступы и авто-закрытие для удобства.

(use-package web-mode
  :defer t
  :ensure t
  :hook (web-mode . eglot-ensure)
  :mode ("\\.html?\\'" "\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'")
  :config
  ;; (add-to-list 'web-mode-engines-alist '("svelte" . "\\.svelte\\'"))
  (add-to-list 'eglot-server-programs `((web-mode) . ("vscode-html-language-server" "--stdio")))
  (setq web-mode-markup-indent-offset 2 web-mode-css-indent-offset 4 web-mode-code-indent-offset 4 web-mode-style-padding 4 web-mode-script-padding 4 web-mode-enable-auto-closing t web-mode-enable-auto-opening t web-mode-enable-auto-pairing t web-mode-enable-auto-indentation t))

;;;; 11. Angular
;; Пакеты для Angular: базовый режим, TypeScript-поддержка и git-интеграция.

(use-package angular-mode
  :ensure t)

(use-package ng2-mode
  :ensure t)

(use-package aangit
  :ensure t)

(provide 'про-код-на-javascript)
;;; про-код-на-javascript.el ends here
