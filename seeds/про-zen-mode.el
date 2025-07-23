;;; про-zen-mode.el --- "Дзен-режим" — чистый fullscreen/editing -*- lexical-binding: t -*-
;;; Commentary:
;; Простой "zen-mode": F11/fullscreen, максимальная тишина, без mode-line и других отвлекающих факторов.
;;; Code:

(defun про/zen-mode (&optional arg)
  "Включить дзен-режим: fullscreen, спрятать mode-line.
ARG отрицательно — выключить."
  (interactive "P")
  (if (or (and arg (< (prefix-numeric-value arg) 1)) (bound-and-true-p про--zen-mode-on))
      (progn
        (setq mode-line-format про--saved-mode-line)
        (when (frame-parameter nil 'fullscreen) (set-frame-parameter nil 'fullscreen nil))
        (setq про--zen-mode-on nil
)
        (message "Дзен-режим выключен"))
    (setq про--saved-mode-line mode-line-format)
    (setq mode-line-format nil)
    (set-frame-parameter nil 'fullscreen 'fullboth)
    (setq про--zen-mode-on t)
    (message "Дзен-режим включён")))

(global-set-key (kbd "<f11>") #'про/zen-mode)

(provide 'про-zen-mode)
;;; про-zen-mode.el ends here
