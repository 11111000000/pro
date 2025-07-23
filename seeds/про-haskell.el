;;; про-haskell.el --- Минималистичная поддержка Haskell -*- lexical-binding: t -*-
;;; Commentary:
;; Поддержка Haskell только если в системе найден ghc или stack.
;;; Code:

(when (or (executable-find "ghc")
          (executable-find "stack"))
  (use-package haskell-mode
    :ensure t
    :mode ("\\.hs\\'" . haskell-mode)
    :hook ((haskell-mode . interactive-haskell-mode)))
  (message "Haskell seed активирован."))

(provide 'про-haskell)
;;; про-haskell.el ends here
