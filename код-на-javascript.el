;;; код-на-javascript.el --- Поддержка Javascript, Typescript
;;; Commentary:
;;; Code:
;;;; Подхватывать настройки проверки кода Javascript

(use-package eslint-rc
  :ensure t
  :custom ((eslint-rc-use-package-json t)
          (eslint-rc-use-eslintignore t)
          (eslint-rc-use-node-modules-bin t))

  :hook ((typescript-ts-mode . eslint-rc-mode)
       (tsx-ts-mode . eslint-rc-mode)
       (js-mode . eslint-rc-mode)
       (web-mode . eslint-rc-mode)))

;;;; Добавить путь для модулей

(use-package add-node-modules-path :ensure t)

;;;; Поддержка версий ноды

(use-package nvm :ensure t)

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

;; (use-package typescript-mode :disabled t)

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :config
  (require 'eglot)
  ;; (setq-default
  ;;  eglot-server-programs
  ;;  (cl-substitute-if
  ;;   (cons
  ;;    '(js-base-mode typescript-ts-base-mode typescript-ts-mode typescript-mode ts-ts-mode)
  ;;    '("/home/az/.nvm/versions/node/v16.20.2/bin/typescript-language-server" "--stdio" :initializationOptions
  ;;      (:preferences
  ;;       (
  ;;        :includeInlayEnumMemberValueHints t
  ;;        :includeInlayFunctionLikeReturnTypeHints t
  ;;        :includeInlayFunctionParameterTypeHints t
  ;;        :includeInlayParameterNameHints "all"
  ;;        :includeInlayParameterNameHintsWhenArgumentMatchesName t
  ;;        :includeInlayPropertyDeclarationTypeHints t
  ;;        :includeInlayVariableTypeHints t
  ;;        :includeInlayVariableTypeHintsWhenTypeMatchesName t))))
  ;;   (lambda (program)
  ;;     (if (listp (car program))
  ;;         (member 'typescript-ts-mode (car program))
  ;;       (eq 'typescript-ts-mode program)))
  ;;   eglot-server-programs))
  
  (add-to-list 'eglot-server-programs
             `((js-mode js-ts-mode tsx-ts-mode typescript-ts-mode typescript-mode) .
               ("/home/az/.nvm/versions/node/v16.20.2/bin/typescript-language-server"
                "--stdio")))
  :hook
  ((typescript-ts-mode . eglot-ensure)
   (js-ts-mode . eglot-ensure)))

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



(provide 'код-на-javascript)
;;; код-на-javascript.el ends here.
