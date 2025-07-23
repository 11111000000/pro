;;; загрузить.el --- Загрузить модуль -*- lexical-binding: t -*-
;;; Commentary:
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

(defun загрузить-org (org-file)
  "Load ORG-FILE as with `org-babel-load-file', then delete the resulting .el file."
  (let ((el-file (concat (file-name-sans-extension org-file) ".el")))
    (prog1
        (org-babel-load-file org-file)
      (when (file-exists-p el-file)
        (delete-file el-file)))))



(provide 'загрузить)
;;; загрузить.el ends here
