;;; про-редактор.el --- Минималистичные расширения редактора -*- lexical-binding: t -*-
;;; Commentary:
;; Delete-selection-mode, hl-line, быстрый переход по символу, расширение выделения и мини-правила.

;;; Code:

(delete-selection-mode 1)
(global-hl-line-mode 1)

(use-package avy
  :ensure t
  :bind (("M-g g" . avy-goto-char)))

(use-package expand-region
  :ensure t
  :bind (("M-SPC" . er/expand-region)
         ("M-S-SPC" . er/contract-region)))

(provide 'про-редактор)
;;; про-редактор.el ends here
