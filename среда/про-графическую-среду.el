;;; про-графическую-среду.el --- Старт графической среды -*- lexical-binding: t -*-

;;; Commentary:
;; Единственная точка оркестрации EXWM.

;;; Code:

(require 'про-мониторы)
(require 'про-графическую-среду-ядро)
(require 'про-графическую-среду-мониторы)
(require 'про-графическую-среду-ввод)
(require 'про-графическую-среду-трей)
(require 'про-графическую-среду-окна)

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "pro-графическая-среда-start loaded"))

(defun pro/старт-графической-среды ()
  "Поэтапная инициализация графического окружения: мониторы, EXWM, трей, раскладка."
  (interactive)
  (message "PRO: про/старт-графической-среды вызвана")
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "exwm-start" "pro/старт-графической-среды entered"))
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "exwm-start" (format "window-system=%s display=%s" window-system (or (getenv "DISPLAY") ""))))
  (unless pro/графика-initialized
    (setq pro/графика-initialized t)
    (catch 'pro/exwm-start
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "applying monitor layout"))
      (применить-расположение-мониторов)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "initializing monitor mapping"))
      (про-мониторы-инициализировать)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "workspace mapping initialized"))
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "loading exwm core"))
      (pro/exwm-load-core)
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" "after exwm load"))
      (dotimes (i 9)
        (exwm-input-set-key (kbd (format "s-<f%d>" i))
                            `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
        (exwm-input-set-key (kbd (format "S-s-<f%d>" i))
                            `(lambda () (interactive) (message ">%d" ,i)))
        (exwm-input-set-key (kbd (format "s-%d" i))
                            `(lambda () (interactive) (tab-bar-select-tab ,i))))
      (exwm-input-set-key (kbd "s-<f10>")
                          `(lambda () (interactive) (exwm-workspace-switch-create 0)))
      (condition-case err
          (pro/exwm-wm-mode-safe)
        (error
         (setq pro/графика-initialized nil)
         (when (fboundp 'pro/log-startup-stage)
           (pro/log-startup-stage "exwm-error" (format "%S" err)))
         (message "PRO: exwm-wm-mode ОШИБКА: %S" err)
         (throw 'pro/exwm-start nil)))
      (when (fboundp 'pro/log-startup-stage)
        (pro/log-startup-stage "exwm-start" (format "after wm-mode booted=%s" pro/exwm-booted)))
      (pro/exwm-setup-window-hooks)
      (pro/exwm-configure-randr)
      (pro/exwm-configure-input-methods)
      (pro/exwm-configure-touchpad)
      (setq exwm-manage-configurations
            `(((or (string= exwm-class-name "Blueman-manager")
                   (string= exwm-class-name "Blueman-applet")
                   (and exwm-title (string-match "blueman" exwm-title)))
               floating nil
               managed t)
              ((equal exwm-title "posframe") floating t floating-mode-line nil)
              ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
               height 175 x 30 y 30 managed t)))
      (setq pro/exwm-booted nil)
      (add-hook 'exwm-init-hook
                (lambda ()
                  (message "PRO: exwm-init-hook вызван!")
                  (when (fboundp 'pro/log-startup-stage)
                    (pro/log-startup-stage "exwm-init-hook" "fired"))
                  (setq pro/exwm-booted t)
                  (when (fboundp 'pro/log-startup-stage)
                    (pro/log-startup-stage "exwm-init-hook" (format "booted=%s" pro/exwm-booted)))
                  (pro/exwm-import-display-env)
                  (pro/exwm-log-systemtray-state "init-hook-before")
                  (pro/exwm-configure-systemtray)
                  (pro/exwm-enable-systemtray)
                  (pro/exwm-log-systemtray-state "init-hook-after")
                  (pro/exwm-rename-all-buffers))))))

(when (fboundp 'pro/startup-log)
  (pro/startup-log "module" "про-графическую-среду load finished"))

(when (fboundp 'pro/log-startup-stage)
  (pro/log-startup-stage "exwm-start" "pro-графическую-среду module loaded"))

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
