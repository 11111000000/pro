;;; про-visual-minimal.el --- Seed: Минимализм визуального, красота пустоты -*- lexical-binding: t -*-
;;; Commentary:
;; Отключает все distracing UI, текущая строка-линией, цвет zen-theme, отсутствие mode-line при idle.
;;; Code:

(defun про/hide-mode-line-in-empty-buffers ()
  "Скрыть mode-line, если буфер пуст, для истинной пустоты."
  (when (zerop (buffer-size))
    (setq mode-line-format nil)))
(add-hook 'window-configuration-change-hook #'про/hide-mode-line-in-empty-buffers)

;; Цветовая схема: tao-yin, если доступна
(ignore-errors (load-theme 'tao-yin t))

;; Подсветить только активную строку
(global-hl-line-mode 1)

(provide 'про-visual-minimal)
;;; про-visual-minimal.el ends here
