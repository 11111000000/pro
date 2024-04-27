;;; про-код-на-javascript.el --- Поддержка Javascript, Typescript
;;; Commentary:
;;; Code:
;;;; Подхватывать настройки проверки кода Javascript

;; ...Неверно работает модуль - сразу eslint fix вешает, убрать его...

;; (use-package eslint-rc
;;   :ensure t
;;   :custom ((eslint-rc-use-package-json t)
;;           (eslint-rc-use-eslintignore t)
;;           (eslint-rc-use-node-modules-bin t))

;;   :hook ((typescript-ts-mode . eslint-rc-mode)
;;        (tsx-ts-mode . eslint-rc-mode)
;;        (js-mode . eslint-rc-mode)
;;        (web-mode . eslint-rc-mode)))

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

;; (defun skewer-location-reload () (interactive) (skewer-eval "window.location.reload()"))

;; (use-package skewer-mode
;;   :ensure t
;;   :bind (("C-c r" . skewer-location-reload)))

;; (use-package prettier-js
;;   :ensure t
;;   :hook ((js-mode . prettier-js-mode)
;;          (web-mode . prettier-js-mode)
;;          (css-mode . prettier-js-mode)
;;          (json-mode . prettier-js-mode)
;;          (typescript-mode . prettier-js-mode)))

;; (defun use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'rjsx-mode-hook #'use-eslint-from-node-modules)

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
                  :quotePreference "single"
                  )
                 )
                )
               )
             )
  ;;  :hook
  ;;(
  ;; (typescript-ts-mode . eglot-ensure)
  ;; (js-ts-mode . eglot-ensure)
  ;;)
  )

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
      (flymake-eslint-enable)))
  
  ;;:hook
  ;; (typescript-ts-mode . попробовать-flymake-eslint)
  ;; (js-ts-mode . попробовать-flymake-eslint)
  ;;)
  )
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/quick-lint-js")

;; (require 'flymake-quicklintjs)

;; (defun попробовать-flymake-quicklintjs ()
;;   "Настройка `quicklintjs' c `flymake'."
;;   (unless (bound-and-true-p flymake-mode)
;;     (flymake-mode))
;;   (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)
;;   (setq-local flymake-no-changes-timeout 0)
;;   (message "Flymaje QuicklintJS")
;;   )

;; (add-hook 'js-ts-mode-hook #'попробовать-flymake-quicklintjs)
;; (add-hook 'typescript-ts-mode-hook #'попробовать-flymake-quicklintjs)

;; (use-package eslint-fix
;;   :hook (typescript-ts-mode . (lambda ()
;;                               (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json")))
;;                                 (setq-local eslint-fix-executable (file-name-concat root "node_modules" ".bin/eslint")))))
;;   :ensure t
;;   :init
;;   )


;; (require 'eglot-quicklintjs)

;; (use-package eglot-quicklintjs
;;   :init)


;; (use-package tide
;;   :ensure t
;;   :after (flycheck)
;;   :hook ((typescript-ts-mode . tide-setup)
;;        (tsx-ts-mode . tide-setup)
;;        (typescript-ts-mode . tide-hl-identifier-mode)
;;        (before-save . tide-format-before-save)))

;; (use-package tsi
;;   :ensure t
;;   :after tree-sitter
;;   ;;:quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; (use-package ts-comint
;;   :ensure t
;;   :bind (:map typescript-mode-map
;;               ( "C-x C-e" . ts-send-last-sexp)
;;               ( "C-c C-c" . ts-send-buffer)))

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
;;; код-на-javascript.el ends here.
