;;; про-lsp.el --- "Дао" LSP: минимально, только если lsp-mode есть в манифесте -*- lexical-binding: t -*-
;;; Commentary:
;; LSP автоматически работает для поддерживаемых языков, никаких глобальных навязываний.
;;; Code:

(when (require 'lsp-mode nil 'noerror)
  (setq lsp-enable-snippet t)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'haskell-mode-hook #'lsp)
  (message "LSP (lsp-mode) seed активирован."))

(provide 'про-lsp)
;;; про-lsp.el ends here
