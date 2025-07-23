;;; про-yasnippet.el --- Yasnippet: минимальное шаблонирование -*- lexical-binding: t -*-
;;; Commentary:
;; Seed для быстрой работы со сниппетами.
;;; Code:

(use-package yasnippet
  :ensure t
  :init (yas-global-mode 1)
  :config
  (message "Yasnippet seed активирован."))

(provide 'про-yasnippet)
;;; про-yasnippet.el ends here
