;;; про-код-на-lisp.el --- LISP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'установить-из)

;;;; Выполнить регион или буфер

(defun выполнить-регион-или-буфер ()
  "выполнить регион или буфер."
  (interactive)
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (перевыполнить-буфер)))

;;;; Emacs Lisp

(use-package emacs
  :defer t
  :bind (:map emacs-lisp-mode-map
                ("C-c C-c" . выполнить-регион-или-буфер)
                ("C-x M-e" . eval-print-last-sexp)))

;;;;; Отладка объектов

(setq eval-expression-print-level 15)
(setq eval-expression-print-length 30)

;;;;; Оптимизация хвостовой рекурсии

(use-package tco
  :defer t  :ensure t)

;;;;; Асинхронные функции

(use-package async-await
  :defer t  :ensure t)

;;;;; Функция для выполнения форм в буфере, включая определения переменных

(defun перевыполнить-буфер ()
  "Вызвать выполнение кода в текущем буфере.
Верхне-уровневые формы выполняются `eval-defun', таким
образом `defvar' и `defcustom' устанавливаются заново."
  (interactive)
  (save-excursion (goto-char (point-min))
              (while (not (eobp))
                (forward-sexp)
                (eval-defun nil))))

;;;;; Конфигурация проверки синтаксиа для Elisp

;; (use-package elsa
;;   :ensure t
;;   )

(use-package flymake-elisp-config
  :defer t 
  :init (установить-из :repo "ROCKTAKEY/flymake-elisp-config")
  :functions (flymake-elisp-config-global-mode flymake-elisp-config-auto-mode)
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))

;;;; Автоматическое тестирование и отладка конфига Emacs

(use-package bug-hunter
  :disabled t
  :ensure t
  :defer t)

;;;; Инспектор объектов ELISP

(use-package inspector
  :defer t  :ensure t)

;;;; Форматирование ELISP

(use-package elisp-format
  :defer t 
  :ensure t
  ;; :hook
  ;; ((emacs-lisp-mode
  ;;         (lambda
  ;;           ()
  ;;           (add-hook 'before-save-hook #'elisp-format-buffer))))
  )

;; (use-package elisp-autofmt
;;   :load-path "emacs-lisp/emacs-lisp-autofmt/"
;;   :commands (elisp-autofmt-mode)
;;   :hook (emacs-lisp-mode . elisp-autofmt-mode))

;;;; REPL к разным LISP-ам Geiser

(use-package geiser
  :defer t 
  :ensure t
  :defines (geiser-mode-start-repl-p)
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  :config
  (setq geiser-mode-start-repl-p nil))

;; Поддержка Geiser специально для Guile Scheme

(use-package geiser-guile
  :defer t 
  :ensure t
  :defines (geiser-guile-manual-lookup-nodes)
  :requires geiser
  :config
  ;;(add-to-list 'geiser-guile-load-path "~/Workspace/guix")
  (setq geiser-guile-manual-lookup-nodes
	   '("guile"
         "guix")))

;;;; Разворачивание макросов

(use-package macrostep
  :defer t 
  :ensure t
  :custom-face
  (macrostep-expansion-highlight-face
   ((t (
         :inherit default
         :extend t
         :background ,(face-attribute 'default :background)))))
  :bind (:map emacs-lisp-mode-map
                ("C-c e" . macrostep-expand)
                :map lisp-interaction-mode-map
                ("C-c e" . macrostep-expand)))

(require 'тихо-применить)

(use-package eros
  :defer t  :ensure t
  :defines (eros-eval-last-sexp)
  :functions (eros-mode)
  :config
  (eros-mode t)
  ;;(global-set-key [remap eval-last-sexp] (lambda () (interactive) (eros-eval-last-sexp)))
  ;;(global-set-key [remap eval-defun] #'eros-eval-defun)
  )

(use-package elisp-docstring-mode
  :defer t  :ensure t)

(use-package format-all
  :defer t  :ensure t :hook (elisp-mode . format-all-mode))

(use-package
  inspector
  :init (установить-из :repo "mmontone/emacs-inspector"))

(provide 'про-код-на-lisp)
;;; про-код-на-lisp.el ends here

