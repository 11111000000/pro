;;; тихо-применить.el --- Подавление сообщений при выполнении функций -*- lexical-binding: t; -*-
;;
;; Автор: az
;; Версия: 1.0
;; Keywords: message, suppress, quiet
;; URL: https://github.com/username/emacs.d/blob/main/инфраструктура/тихо-применить.el
;;
;;; Commentary:
;;
;; Утилита для временного подавления вывода message при вызове функций.
;; Полезно при программном выполнении операций без лишнего шума.
;;
;;; Code:

(require 'cl-macs)

(defun тихо-применить (old-fun &rest args)
  "Применить и приглушить сообщения функции OLD-FUN с аргументами ARGS."
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply old-fun args)
      (advice-remove 'message #'silence))))

(provide 'тихо-применить)
;;; тихо-применить.el ends here
