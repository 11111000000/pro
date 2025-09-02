;;; про-функции.el --- Полезные вспомогательные функции -*- lexical-binding: t; -*-
;;; Commentary:
;; В этом файле размещаются различные вспомогательные команды и функции,
;; которые могут использоваться в других частях про-конфига.

;;; Code:

(defun про/current-md-to-org ()
  "Конвертировать текущий .md-файл в .org с помощью pandoc."
  (interactive)
  (let* ((md-file (buffer-file-name))
         (ext (file-name-extension md-file))
         (org-file (concat (file-name-sans-extension md-file) ".org")))
    (unless (and md-file (string= ext "md"))
      (user-error "Текущий буфер не markdown-файл"))
    (when (file-exists-p org-file)
      (unless (yes-or-no-p (format "Файл %s уже существует. Перезаписать? " org-file))
        (user-error "Операция отменена пользователем.")))
    (let ((cmd (format "pandoc -f markdown -t org '%s' -o '%s'" md-file org-file)))
      (message "Выполняется: %s" cmd)
      (shell-command cmd)
      (find-file-other-window org-file)
      (message "Конвертация завершена: %s" org-file))))

(provide 'про-функции)
;;; про-функции.el ends here
