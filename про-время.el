;;; про-время.el --- Время и календарь -*- lexical-binding: t -*-
;;; Commentary:

;; Время и календарь

;;; Code:

(defun set-calendar-font ()
  "Шрифт для календаря."
  (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
  (buffer-face-mode t))

(use-package calendar
  :defer t 
  :hook ((calendar-mode . set-calendar-font)))

(provide 'про-время)

;;; про-время.el ends here
