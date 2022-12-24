;;; загрузить.el --- Загрузить модуль
;;; Commentary
;;; Code:

(defun загрузить (feature &optional filename) 
  "Как `require', но с аккуратной обработкой ошибок.

Если возникнет ошибка, её текст будет добавлен в сообщение.

Как `require', вернёт FEATURE если всё ок, и nil если нет."
  (condition-case err 
      (require feature filename) 
    (error 
     (message "Ошибка загрузки %s: \"%s\"" (if filename (format "%s (%s)" feature filename) feature) 
              (error-message-string err))
     nil)))

(provide 'загрузить)
;;; загрузить.el ends here
