;;; программирование-на-javascript.el --- Поддержка Javascript, Typescript
;;; Commentary:
;;; Code:
;;;; Javascript

(leaf eslint-rc
  :ensure t
  :custom ((eslint-rc-leaf-json . t)
           (eslint-rc-use-eslintignore . t)
           (eslint-rc-use-node-modules-bin . t))
  :hook ((typescript-mode . eslint-rc-mode)
         (typescriptreact-mode . eslint-rc-mode)
         (js-mode . eslint-rc-mode)
         (web-mode . eslint-rc-mode)))

(leaf add-node-modules-path :ensure t)

(leaf nvm :ensure t)

(leaf js-doc
  :ensure t  
  :bind ((:js2-mode-map
          ("C-c jD" . js-doc-insert-file-doc)
          ("C-c jd" . js-doc-insert-function-doc)))
  :config)

(leaf react-snippets :ensure t :init (require 'react-snippets))

(leaf json-mode
  :ensure t)

;; (defun skewer-location-reload () (interactive) (skewer-eval "window.location.reload()"))

;; (leaf skewer-mode
;;   :ensure t
;;   :bind (("C-c r" . skewer-location-reload)))

;; (leaf prettier-js
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

(leaf typescript-mode
  :ensure t
  :after tree-sitter  
  :config  
  (define-derived-mode typescript-react-mode typescript-mode
    "Typescript JSX")

  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-react-mode))    
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-react-mode . tsx)))

;; (leaf tsi
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

(leaf ts-comint 
  :ensure t
  :bind ((:typescript-mode-map
          ( "C-x C-e" . ts-send-last-sexp)
                                        ;( "C-M-x" . ts-send-last-sexp-and-go)
          ( "C-c C-c" . ts-send-buffer)
                                        ;( "C-c C-b" . ts-send-buffer-and-go)
                                        ;( "C-c l" . ts-load-file-and-go)
          )))

(provide 'программирование-на-javascript)
;;; программирование-на-javascript.el ends here.
