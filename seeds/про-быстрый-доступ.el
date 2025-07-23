;;; про-быстрый-доступ.el --- Быстрый поиск и удобное переключение буферов -*- lexical-binding: t -*-
;;; Commentary:
;; Минимальный seed для быстрого доступа: vertico, orderless, consult (поиск, открытие буферов).
;;; Code:

;; Современное вертикальное дополение для M-x, find-file, switch-to-buffer и пр.
(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

;; Мощный стиль фильтрации
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Подписи к кандидантам (M-x marginalia)
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode 1))

;; Быстрый вертикальный поиск/переключение буферов и файлов
(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-s" . consult-line)
         ("M-y" . consult-yank-pop)
         ("M-g g" . consult-goto-line))
  :config
  (setq consult-preview-key "M-."))

(provide 'про-быстрый-доступ)
;;; про-быстрый-доступ.el ends here
