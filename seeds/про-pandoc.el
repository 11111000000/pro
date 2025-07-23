;;; про-pandoc.el --- Интеграция с Pandoc для конвертации документов -*- lexical-binding: t -*-
;;; Commentary:
;; Даосская интеграция: pandoc-mode не активируется, если не найден pandoc в окружении.
;;; Code:

(when (executable-find "pandoc")
  (use-package pandoc-mode
    :ensure t
    :hook ((markdown-mode org-mode latex-mode) . pandoc-mode))
  (message "Pandoc seed активирован."))

(provide 'про-pandoc)
;;; про-pandoc.el ends here
