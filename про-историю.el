;;; про-историю.el --- История -*- lexical-binding: t -*-
;;; Commentary:
;; Функции для работы с историей, хранением и
;; восстановлением состояний
;;; Code:
;;;; Сохранение истории

(use-package no-littering
  :ensure t
  :custom ((make-backup-files t)
          (delete-by-moving-to-trash t)
          (backup-by-copying t)
          (kept-new-versions 25)
          (history-delete-duplicates t)
          (history-length 300)
          (savehist-autosave-interval 300)
          (kept-old-versions 25)
          (delete-old-versions t)
          (create-lockfiles nil)
          (vc-make-backup-files t)
          (version-control t))
  :functions (no-littering-expand-var-file-name no-littering-expand-etc-file-name no-littering-theme-backups)
  :config
  (savehist-mode t)
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;; Сохранять действия в ~~/.emacs.d/history~

;;;; Текстовая Машина Времени
;;;;; Дерево версий текста

(use-package undo-tree
  :ensure t
  :diminish " ⸙"
  :functions (global-undo-tree-mode)
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/undo/"))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff nil)

  :bind (("C-M--" . undo-tree-visualize)
         ("C-M-_" . undo-tree-visualize)
         ("M-u" . undo-tree-visualize))
  :init (global-undo-tree-mode 1))


;;;;; Вернуться к последней правке

(use-package goto-last-change
  :defer t
  :ensure t
  :bind (("C-c C-," . goto-last-point)))

;;;;; Вернуться к предыдущей позиции курсора

(use-package goto-last-point
  :defer t 
  :ensure t
  :functions (goto-last-point-mode)
  :bind (("C-c ," . goto-last-point))
  :config
  (goto-last-point-mode t))

;; Сохранение положения

(use-package eyebrowse
  :defer t 
  :ensure t
  :functions (eyebrowse-mode)
  :config (eyebrowse-mode))

;;;; Помнить места

(use-package saveplace
  :ensure t
  :after (no-littering)
  :init
  (save-place-mode t))

;;;; Помнить недавние файлы

(use-package recentf
  :after (no-littering)
  :custom ((recentf-max-saved-items 512)   ;; всего
          (recentf-max-menu-items 100)    ;; меню

          ;; ...исключая некоторые:

          (recentf-exclude '("/\\.git/.*\\'"      ; Git contents
                             "/\\.emacs\\.d/elpa" ; ELPA
                             "-autoloads\\.el\\'"
                             no-littering-var-directory
                             no-littering-etc-directory
                             "\\.elc\\'"
                             "/TAGS\\'"))
          )
  :config
  (recentf-mode t))

;;;; История копирования

(setq-default kill-ring-max 300
         save-interprogram-paste-before-kill t)

;;;; Сохранение сессии

;; (desktop-save-mode t)

;; Но не загружаем его

;;(ignore-errors (load custom-file))

;;;; Синхронизация буфера обмена и kill ring

(setq-default save-interprogram-paste-before-kill t)
(setq-default yank-pop-change-selection t)
(setq-default x-select-enable-primary t)


(provide 'про-историю)
;;; про-историю.el ends here
