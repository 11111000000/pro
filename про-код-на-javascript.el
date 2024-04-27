;;; про-код-на-javascript.el --- Поддержка Javascript, Typescript
;;; Commentary:
;;; Code:

;;;; Добавить путь для модулей

(use-package add-node-modules-path :ensure t)

;;;; Поддержка разных версий ноды

(use-package nvm
  :ensure t
  :functions (nvm-use)
  :init
  (nvm-use "16")
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

;;;;; Навигация по JSON

(use-package json-navigator
  :ensure t)


;;;; Typescript

;; В Emacs теперь есть встроеная поддержка typescript-ts-mode

(require 'eglot)

(use-package typescript-ts-mode
  :after (eglot)
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config

  ;; Добавляем typescript-language-server
  
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
                  :includeInlayParameterNameHints "literals" ; "none" | "literals" | "all"
                  :includeInlayParameterNameHintsWhenArgumentMatchesName t
                  :includeInlayPropertyDeclarationTypeHints t
                  :includeInlayVariableTypeHints t
                  :includeInlayVariableTypeHintsWhenTypeMatchesName t
                  :organizeImportsCaseFirst "upper"
                  :organizeImportsCollation "unicode" ; "ordinal" | "unicode"
                  :organizeImportsIgnoreCase :json-false
                  :quotePreference "single"))))))

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

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/quick-lint-js")

;;;; JTSX

;; Расширеная поодержка JSX: https://github.com/llemaitre19/jtsx

;;;; GraphQL

(use-package graphql-mode
  :ensure t)

(use-package graphql-ts-mode :ensure t)

;;;; Сниппеты React

(use-package react-snippets :ensure t :init (require 'react-snippets))

;;;; Генерация схем

(defun typescript-show-uml ()
  "показать UML."
  (interactive)
  (message (concat "tsuml2 -m --glob \"" (buffer-file-name) "\" -o "
                (concat (buffer-file-name) ".png"))))

;;;; CSV

(use-package csv-mode :ensure t)

(provide 'про-код-на-javascript)
;;; про-код-на-javascript.el ends here.
