;;; про-код-на-haskell.el --- Haskell -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация для разработки на Haskell
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
  :custom
  (haskell-process-type 'cabal-repl)
  (haskell-process-auto-import-loaded-modules t)
  (haskell-process-log t)
  (haskell-stylish-on-save t)
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  :bind
  (:map haskell-mode-map
        ("C-c C-l" . haskell-process-load-file)
        ("C-c C-z" . haskell-interactive-switch)
        ("C-c C-k" . haskell-interactive-mode-clear)
        ("C-c C-t" . haskell-mode-show-type-at)
        ("C-c C-i" . haskell-process-do-info))
  :config

  (add-to-list 'eglot-server-programs '(haskell-mode "haskell-language-server-wrapper" "--lsp")))

(provide 'про-код-на-haskell)
;;; про-код-на-haskell.el ends here.
