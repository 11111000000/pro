;;; про-markdown.el --- Поддержка Markdown -*- lexical-binding: t -*-
;;; Commentary:
;; Включает markdown-mode, если найден бинарник markdown (pandoc, markdown) или пакет установлен через manifest.
;;; Code:

(when (or (executable-find "markdown")
          (executable-find "pandoc"))
  (use-package markdown-mode
    :ensure t
    :mode ("\\.md\\'" . markdown-mode))
  (message "Markdown seed активирован."))

(provide 'про-markdown)
;;; про-markdown.el ends here
