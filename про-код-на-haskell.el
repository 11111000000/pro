;;; про-код-на-haskell.el --- RUST -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Rust mode

(use-package haskell-mode
  :defer t
  :ensure t
  :hook ((haskell-mode . subword-mode)
  (haskell-mode . interactive-haskell-mode)
  (haskell-mode . haskell-doc-mode)))



(provide 'про-код-на-haskell)
;;; про-код-на-haskell.el ends here.
