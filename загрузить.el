;;; загрузить.el --- Загрузить модуль
;;; Commentary:
;;; Code:

(defun загрузить (feature &optional filename)
  "Загружает FEATURE из FILENAME, как `require', но с аккуратной обработкой ошибок.
Если возникнет ошибка, её текст будет отображен в сообщении.
Вернёт FEATURE если всё ок, и nil если файла нет, или другие ошибки возникли."
  (condition-case err
      (require feature filename)
    (error
     (message "Ошибка загрузки %s: \"%s\"" (if filename (format "%s (%s)" feature filename) feature)
              (error-message-string err))
     nil)))

(provide 'загрузить)
;;; загрузить.el ends here
