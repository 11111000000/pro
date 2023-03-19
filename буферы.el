;;; буферы.el --- Управление буферами
;; Управление буферами
;;; Commentary:
;; Управление буферами происходит в следующем порядке....
;;; Code:
;;;; Буферы

;; Уникальные имена для буферов

(setq uniquify-buffer-name-style 'forward)

;; Включаем автоактуализацию всех буферов

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; Обновление буфера без вопросов

(defun обновить-буфер-немедленно ()
  "ничего не спрашивая, обновить буфер."
  (interactive)
  (revert-buffer t t))

;; Асинхронные буферы скрыты

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; Новые асинхронные буферы переименовываются не спрашивая ничего:

(setq-default async-shell-command-buffer 'rename-buffer)

;; Буфер с ошибками только при ошибках

(setq warning-minimum-level :error)

;; Скроллбар

(if window-system (scroll-bar-mode -1))

(provide 'буферы)
;;; буферы.el ends here
