;;; про-графическую-среду-трей.el --- Трей EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Управление system tray и связанным окружением.

;;; Code:

(defun pro/exwm-log-systemtray-state (&optional stage)
  "Log the current EXWM system tray state."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "systemtray" (format "begin %s" (or stage "?"))))
  (let ((target (string-trim
                 (shell-command-to-string
                  "systemctl --user is-active exwm-session.target 2>/dev/null || true"))))
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "systemtray" (format "after systemctl %s" (or stage "?"))))
    (message "PRO: systemtray[%s] feature=%s mode=%s booted=%s target-active=%s"
             (or stage "?")
             (featurep 'exwm-systemtray)
             (and (boundp 'exwm-systemtray-mode) exwm-systemtray-mode)
             (and (boundp 'pro/exwm-booted) pro/exwm-booted)
             (if (string-empty-p target) "unknown" target))))

(defun pro/exwm-systemtray-mode-advice (orig-fn &rest args)
  "Перехватывает вызовы exwm-systemtray-mode для диагностики."
  (message "PRO: exwm-systemtray-mode вызван: args=%s" args)
  (let ((val (apply orig-fn args)))
    (message "PRO: exwm-systemtray-mode=%s после вызова" exwm-systemtray-mode)
    val))

(advice-add 'exwm-systemtray-mode :around #'pro/exwm-systemtray-mode-advice)

(defun pro/exwm-configure-systemtray ()
  "Подготовить окружение для system tray."
  (pro/exwm-import-display-env))

(defun pro/exwm-enable-systemtray ()
  "Включить системный трей и запустить user target для EXWM-сессии."
  (message "PRO: exwm-enable-systemtray вызвана")
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "systemtray" "before enable-systemtray"))
  (pro/exwm-log-systemtray-state "enable-before")
  (require 'exwm-systemtray nil t)
  (when (fboundp 'exwm-systemtray-mode)
    (message "PRO: exwm-systemtray-mode ДО=%s" exwm-systemtray-mode)
    (exwm-systemtray-mode t)
    (message "PRO: exwm-systemtray-mode ПОСЛЕ=%s" exwm-systemtray-mode))
  (pro/exwm-log-systemtray-state "enable-after")
  (run-at-time 1 nil
               (lambda ()
                 (pro/exwm-log-systemtray-state "enable-delayed")
                 (ignore-errors
                   (start-process "systemctl-user-reset-failed" nil
                                  "systemctl" "--user" "reset-failed")
                   (start-process "systemctl-user-start" nil
                                  "systemctl" "--user" "start" "exwm-session.target")))))

(provide 'про-графическую-среду-трей)
;;; про-графическую-среду-трей.el ends here
