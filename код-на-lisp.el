;;; код-на-lisp.el --- LISP
;;; Commentary:
;;; Code:

(require 'use-package)


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

(use-package emacs-lisp
  :bind (:map emacs-lisp-mode-map
                ("C-c C-c" . выполнить-регион-или-буфер)
                ("C-x M-e" . eval-print-last-sexp)))

;;;; Функция для выполнения форм в буфере, включая определения переменных

(defun перевыполнить-буфер ()
  "Вызвать выполнение кода в текущем буфере.
Верхне-уровневые формы выполняются `eval-defun', таким
образом `defvar' и `defcustom' устанавливаются заново."
  (interactive)
  (save-excursion (goto-char (point-min))
              (while (not (eobp))
                (forward-sexp)
                (eval-defun nil))))

;;;; Конфигурация проверки синтаксиа для Elisp

;; (use-package elsa
;;   :ensure t
;;   )

;; (use-package flycheck-elsa
;;   :ensure t
;;   :after flycheck
;;   :config (flycheck-elsa-setup))

(require 'менеджер-пакетов)

(use-package flymake-elisp-config
  :init (установить-из :repo "ROCKTAKEY/flymake-elisp-config")
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))

;;;; Автоматическое тестирование и отладка конфига Emacs

(use-package bug-hunter
  :disabled t
  :ensure t
  :defer t)

;;;; Инспектор объектов ELISP

(use-package inspector :ensure t)

;;;; Форматирование ELISP

(use-package elisp-format
  :ensure t
  ;; :hook
  ;; ((emacs-lisp-mode
  ;;         .
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
  :ensure t
  :requires geiser
  :config
  ;;(add-to-list 'geiser-guile-load-path "~/Workspace/guix")
  (setq geiser-guile-manual-lookup-nodes
	  '("guile"
        "guix")))

;;;; Рефакторинг Emacs lisp

;; (use-package erefactor
;;   :ensure t
;;   :after (flymake))

;;;; Редактирование лиспа

;; (use-package lispy
;;   :ensure t
;;   ;; :hook
;;   ;; (emacs-lisp-mode . lispy-mode)
;;   )

;;;; Подсветка вызовов функций

;; (use-package highlight-function-calls
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . highlight-function-calls-mode))

;;;; Разворачивание макросов

(use-package macrostep
  :ensure t
  :custom-face
  (macrostep-expansion-highlight-face ((t (:inherit default :extend t :background "#222"))))
  :bind (:map emacs-lisp-mode-map
                ("C-c e" . macrostep-expand)
                :map lisp-interaction-mode-map
                ("C-c e" . macrostep-expand)))

(require 'тихо-применить)

(use-package eros :ensure t
  :defines (eros-eval-last-sexp)

  :config
  (eros-mode t)
                                        ;(global-set-key [remap eval-last-sexp] (lambda () (interactive) (eros-eval-last-sexp)))
                                        ;(global-set-key [remap eval-defun] #'eros-eval-defun)
  )

(use-package elisp-docstring-mode :ensure t)

(use-package format-all :ensure t :hook (elisp-mode . format-all-mode))

(use-package
  inspector
  :init (установить-из :repo "mmontone/emacs-inspector"))

(provide 'код-на-lisp)
;;; код-на-lisp.el ends here
