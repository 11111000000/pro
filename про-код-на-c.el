;;; про-код-на-c.el --- C -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; C# mode

(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode)))

(provide 'про-код-на-c)
;;; про-код-на-c.el ends here
