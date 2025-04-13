;;; про-код-на-haskell.el --- Haskell -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация для разработки на Haskell в Emacs с полезными настройками.
;;; Code:

;;;; Haskell mode

(require 'eglot)

(use-package haskell-mode
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
       ;;(haskell-mode . subword-mode)
       (haskell-mode . interactive-haskell-mode)
       ;;(haskell-mode . haskell-indentation-mode)
       (haskell-mode . haskell-decl-scan-mode)
       (haskell-mode . haskell-doc-mode)
       (haskell-mode . yas-minor-mode))
  ;; :hook
  ;; ((haskell-mode . eglot-ensure)
  ;; (haskell-mode . subword-mode)
  ;; ;; (haskell-mode . interactive-haskell-mode)
  ;; (haskell-mode . haskell-doc-mode)
  ;; (haskell-mode . (lambda () (eglot-inlay-hints-mode -1)))
  ;; (haskell-mode . haskell-indentation-mode))
  :custom
  ;; Предпочитаем запуск через cabal-repl; можно изменить тип процесса при необходимости.
  (haskell-process-type 'cabal-repl)
  ;; Автоматически импортировать модули, загруженные в REPL.
  (haskell-process-auto-import-loaded-modules t)
  ;; Ведение лога работы процесса для отладки.
  (haskell-process-log t)
  ;; Запуск форматирования кода при сохранении с использованием stylish-haskell.
  (haskell-stylish-on-save t)
  ;; Настройка отступов для удобного форматирования кода.
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  :config

  (add-to-list 'eglot-server-programs '(haskell-mode "haskell-language-server-wrapper" "--lsp")))

(provide 'про-код-на-haskell)
;;; про-код-на-haskell.el ends here.
