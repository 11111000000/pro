;;; про-буферы.el --- Буферы -*- lexical-binding: t -*-
;;; Commentary:
;; Буферы - одна из базовых сущностей, в буферах находится всё остальное - файлы и окна приложений...
;;; Code:
;;;; Буферы

;; Каждый буфер называется уникально

(setq uniquify-buffer-name-style 'forward)

;; иногда нужно обновить буфер немедленно

(defun обновить-буфер-немедленно ()
    "ничего не спрашивая, обновить буфер."
    (interactive)
    (revert-buffer t t))

;; но восновном буферы актуализируются автоматически

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; асинхронные буферы скрыты из списка

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; новые асинхронные буферы переименовываются не спрашивая ничего:

(setq-default async-shell-command-buffer 'rename-buffer)

;; буфер с ошибками показывается только при ошибках

(require 'warnings)

(setq warning-minimum-level :error)

;;;; Группировать по проекту

(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :hook (ibuffer . my/ibuffer-projectile-groups)
  :config
  (defun my/ibuffer-projectile-groups ()
    (ibuffer-projectile-set-filter-groups)
    (unless (eq ibuffer-sorting-mode 'alphabetic)
      (ibuffer-do-sort-by-alphabetic))))

(provide 'про-буферы)
;;; про-буферы.el ends here
