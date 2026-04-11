;;; загрузить.el --- Универсальная загрузка модулей с обработкой ошибок -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: load, require, module
;; URL: https://github.com/username/emacs.d/blob/main/инфраструктура/загрузить.el
;;
;;; Commentary:
;;
;; Утилита для безопасной загрузки модулей с красивым выводом в echo-area.
;; Позволяет загружать Org-файлы через org-babel-load-file.
;;
;;; Code:

(defun загрузить (feature &optional filename)
  "Загружает FEATURE из FILENAME, как `require', но с аккуратной обработкой ошибок.
Если возникнет ошибка, её текст будет отображен в сообщении.
Вернёт FEATURE если всё ок, и nil если файла нет, или другие ошибки возникли."
  (condition-case err
      (progn
        (message "%s..." feature)
        (require feature filename))
    (error
     (message "%s %s: \"%s\"" (propertize "Ошибка загрузки" 'face 'highlight) (if filename (format "%s (%s)" feature filename) feature)
              (error-message-string err))
     nil)))

(defun pro/load-module (feature)
  "Load FEATURE and continue on error."
  (condition-case err
      (загрузить feature)
    (error
     (message "Модуль %s упал: %s" feature (error-message-string err))
     nil)))

(defun загрузить-org (org-file)
  "Load ORG-FILE as with `org-babel-load-file', then delete the resulting .el file."
  (let ((el-file (concat (file-name-sans-extension org-file) ".el")))
    (prog1
        (org-babel-load-file org-file)
      (when (file-exists-p el-file)
        (delete-file el-file)))))



(provide 'загрузить)
;;; загрузить.el ends here
