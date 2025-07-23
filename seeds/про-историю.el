;;; про-историю.el --- История, отмена, recentf, savehist -*- lexical-binding: t -*-
;;; Commentary:
;; Минималистичный seed для истории файлов, буфера отмен, позиций и недавних файлов (без засорения).

;;; Code:

(savehist-mode 1)
(recentf-mode 1)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo/" user-emacs-directory)))))

;; Помнить открытые места (simple save-place)
(save-place-mode 1)

(provide 'про-историю)
;;; про-историю.el ends here
