;;; про-графическую-среду-мониторы.el --- Мониторы EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Конфигурация RandR и обновление раскладки экранов.

;;; Code:

(defun pro/exwm-configure-randr ()
  "Включить RandR и обновить конфигурацию экранов."
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "randr" "before configure-randr"))
  (when (fboundp 'exwm-randr-mode)
    (exwm-randr-mode 1)
    (ignore-errors (exwm-randr-refresh)))
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "randr" "after configure-randr")))

(provide 'про-графическую-среду-мониторы)
;;; про-графическую-среду-мониторы.el ends here
