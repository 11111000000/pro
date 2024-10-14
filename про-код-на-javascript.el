;;; про-код-на-javascript.el --- Поддержка Javascript, Typescript -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; Режим Javascript

(require 'eglot)

(use-package js
  :after (eglot)
  :hook ((js-ts-mode . eglot-ensure)
       (js-ts-mode . (lambda () (eglot-inlay-hints-mode -1))))
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :interpreter (("node" . js-ts-mode))
  :init
  
  (add-to-list 'eglot-server-programs
             `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) .
               ("/home/az/.nvm/versions/node/v16.20.2/bin/typescript-language-server"
                "--stdio"
                :initializationOptions
                (:preferences
                 (
                  :importModuleSpecifierPreference "non-relative"
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
                  :organizeImportsIgnoreCase :json-false
                  :quotePreference "single"))))))

;;;; Добавить путь для модулей

(use-package add-node-modules-path :ensure t)

;;;; Поддержка разных версий ноды

(use-package nvm
  :ensure t
  :functions (nvm-use)
  :init
  (nvm-use "18")
  )

;;;; Документация в комментариях

;; (use-package js-doc
;;   :ensure t
;;   :defer t
;;   :bind (:map js2-mode-map
;;               ("C-c jD" . js-doc-insert-file-doc)
;;               ("C-c jd" . js-doc-insert-function-doc))
;;   :config)

;;;; HTTP Запросы

(use-package request
  :ensure t)

;;;; Поддержка JSON

(use-package json-mode
  :ensure t
  :hook ((json-mode . hs-minor-mode)))

;; Навигация по JSON

(use-package json-navigator
  :ensure t)

;;;; Проверка кода на Javascript

(use-package flymake-eslint
  :ensure t
  :functions flymake-eslint-enable
  :preface

  (defun попробовать-flymake-eslint ()
    "Включить `flymake-eslint' основываясь на конфигурации проекта.
Ищет ESLint конфиг проекта, чтобы определить чем проверять буфер."
    (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
      (make-local-variable 'exec-path)
      (push (file-name-concat root "node_modules" ".bin") exec-path)
      (flymake-eslint-enable))))

;;(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/quick-lint-js")

;;;; JTSX

;; Расширеная поодержка JSX: https://github.com/llemaitre19/jtsx

;;;; GraphQL

(use-package graphql-mode
  :ensure t)

(use-package graphql-ts-mode :ensure t)

;;;; Сниппеты React

;;(use-package react-snippets :ensure t :init (require 'react-snippets))

;;;; Генерация схем

(defun typescript-show-uml ()
  "показать UML."
  (interactive)
  (message (concat "tsuml2 -m --glob \"" (buffer-file-name) "\" -o "
                (concat (buffer-file-name) ".png"))))

;;;; CSV

(use-package csv-mode :ensure t)

;;;; HTML и шаблоны

(use-package web-mode
  :ensure t
  :hook (web-mode . eglot-ensure)
  :mode "\\.html?\\'"
  :mode "\\.phtml\\'"
  :mode "\\.tpl\\.php\\'"
  :mode "\\.[agj]sp\\'"
  :mode "\\.as[cp]x\\'"
  :mode "\\.erb\\'"
  :mode "\\.mustache\\'"
  :mode "\\.djhtml\\'"
  :config
  ;;(add-to-list 'web-mode-engines-alist '("svelte" . "\\.svelte\\'"))
  (add-to-list 'eglot-server-programs
             `((web-mode) . ("vscode-html-language-server" "--stdio") ))
  
  (setq
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 4
   web-mode-code-indent-offset 4
   web-mode-style-padding 4
   web-mode-script-padding 4
   web-mode-enable-auto-closing t
   web-mode-enable-auto-opening t
   web-mode-enable-auto-pairing t
   web-mode-enable-auto-indentation t
   ))

(provide 'про-код-на-javascript)
;;; про-код-на-javascript.el ends here.
