;;; время.el --- Время и календарь -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: 1.0.0
;; Package-Requires: (dependencies)
;; Homepage: dobro.ru
;; Keywords: dobro

;;; Commentary:

;; Время и календарь

;;; Code:

(defun set-calendar-font ()
  "Шрифт для календаря."
  (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
  (buffer-face-mode t))

(use-package calendar
  :hook ((calendar-mode . set-calendar-font)))

(provide 'про-время)

;;; время.el ends here
