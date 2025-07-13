;;; про-графическую-среду.el --- Минимальный EXWM Дао (опционально) -*- lexical-binding: t -*-
;; Опциональный старт EXWM: только если переменная PRO2_EXWM или ключ --with-exwm.
(defun про/надо-ли-стартовать-exwm ()
  "Возвращает t, если требуется запуск exwm (флаг $PRO2_EXWM или аргумент --with-exwm)."
  (or (getenv "PRO2_EXWM")
      (and (boundp 'argv) (seq-some (lambda (x) (string= x "--with-exwm")) argv))))
(when (про/надо-ли-стартовать-exwm)
  (when (require 'exwm nil t)
    (setq exwm-workspace-number 1)
    (exwm-enable)
    (message "EXWM: оконный менеджер активирован (по переменной или опции --with-exwm).")))
(provide 'про-графическую-среду)
