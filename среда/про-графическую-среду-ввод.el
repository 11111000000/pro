;;; про-графическую-среду-ввод.el --- Ввод EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; XIM и вспомогательные настройки ввода.

;;; Code:

(defun pro/exwm-configure-input-methods ()
  "Включить XIM и стандартные simulation keys."
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "begin configure-input-methods"))
  (require 'exwm-xim nil t)
  (when (fboundp 'exwm-xim-mode)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "input-methods" "before exwm-xim-mode"))
    (when (fboundp 'pro/log-startup-stage)
      (pro/log-startup-stage "xim" "before exwm-xim-mode"))
    (exwm-xim-mode t))
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "xim" "after exwm-xim-mode"))
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "before set simulation keys"))
  (setq exwm-input-simulation-keys pro/exwm-default-simulation-keys)
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "after set simulation keys"))
  (unless (member ?\C-\\ exwm-input-prefix-keys)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "input-methods" "before push prefix key"))
    (push ?\C-\\ exwm-input-prefix-keys))
  (when (fboundp 'pro/startup-log)
    (pro/startup-log "input-methods" "end configure-input-methods")))

(defun pro/disable-touchpad-while-typing-enable ()
  "Включить Disable-While-Typing для всех тачпадов libinput (X11)."
  (interactive)
  (let ((script
         "if command -v xinput >/dev/null 2>&1; then
  xinput list --name-only | grep -i -E 'touchpad|trackpad|clickpad' | while read dev; do
    if xinput list-props \"$dev\" 2>/dev/null | grep -q 'libinput Disable While Typing Enabled'; then
      xinput set-prop \"$dev\" 'libinput Disable While Typing Enabled' 1 || true
    fi
  done
fi"))
    (start-process-shell-command "dwt-touchpad" nil script)))

(defun pro/exwm-configure-touchpad ()
  "Включить защиту от случайных кликов тачпадом."
  (when (display-graphic-p)
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "touchpad" "begin configure-touchpad"))
    (pro/disable-touchpad-while-typing-enable)
    (with-eval-after-load 'exwm-randr
      (when (fboundp 'pro/startup-log)
        (pro/startup-log "touchpad" "install randr hook"))
      (add-hook 'exwm-randr-screen-change-hook
                #'pro/disable-touchpad-while-typing-enable))
    (when (fboundp 'pro/startup-log)
      (pro/startup-log "touchpad" "end configure-touchpad"))))

(provide 'про-графическую-среду-ввод)
;;; про-графическую-среду-ввод.el ends here
