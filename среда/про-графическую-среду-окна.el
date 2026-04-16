;;; про-графическую-среду-окна.el --- Окна EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Правила именования окон и хуки обновления.

;;; Code:

(defun pro/exwm-browser-p ()
  "Ненулевой результат, если текущее окно похоже на браузер."
  (let ((class (and (boundp 'exwm-class-name) exwm-class-name)))
    (and class
         (member (downcase class) pro/exwm-browser-classes))))

(defun pro/exwm-window-title ()
  "Вернуть заголовок окна EXWM, если он есть."
  (when (and (boundp 'exwm-title) exwm-title (> (length exwm-title) 0))
    exwm-title))

(defun pro/exwm-window-class ()
  "Вернуть класс окна EXWM, если он есть."
  (when (and (boundp 'exwm-class-name) exwm-class-name (> (length exwm-class-name) 0))
    exwm-class-name))

(defun pro/exwm-app-name ()
  "Вернуть имя для EXWM-буфера: title, class, instance или EXWM."
  (or (pro/exwm-window-title)
      (pro/exwm-window-class)
      (and (boundp 'exwm-instance-name) exwm-instance-name)
      "EXWM"))

(defun pro/exwm-rename-buffer ()
  "Переименовать текущий EXWM-буфер."
  (when (derived-mode-p 'exwm-mode)
    (let ((name (pro/exwm-app-name)))
      (when (and name (> (length name) 0))
        (exwm-workspace-rename-buffer name)))))

(defun pro/exwm-rename-all-buffers ()
  "Переименовать все текущие EXWM-буферы по тем же правилам."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'exwm-mode)
        (pro/exwm-rename-buffer)))))

(defun pro/exwm-setup-window-hooks ()
  "Настроить хуки переименования EXWM-окон."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "hooks" "begin setup-window-hooks"))
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "hooks" "before setup-window-hooks"))
  (add-hook 'exwm-update-class-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-update-title-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-manage-finish-hook #'pro/exwm-rename-buffer)
  (add-hook 'exwm-init-hook #'pro/exwm-rename-all-buffers)
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "hooks" "end setup-window-hooks")))

(provide 'про-графическую-среду-окна)
;;; про-графическую-среду-окна.el ends here
