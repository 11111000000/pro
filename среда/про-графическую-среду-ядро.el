;;; про-графическую-среду-ядро.el --- Ядро EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Базовые данные и примитивы графической среды: состояния старта,
;; симуляционные клавиши и безопасный запуск WM.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar pro/графика-initialized nil
  "Истина, если графическая среда уже инициализирована.")

(defvar pro/exwm-booted nil
  "Истина, если EXWM реально поднялся и exwm-init-hook отработал.")

(defgroup pro/графика nil
  "Настройки графической среды EXWM."
  :group 'exwm)

(defconst pro/exwm-default-simulation-keys
  '(([?\C-b] . left)
    ([?\M-b] . C-left)
    ([?\C-f] . right)
    ([?\M-f] . C-right)
    ([?\C-p] . up)
    ([?\C-n] . down)
    ([?\C-a] . home)
    ([?\C-e] . end)
    ([?\M-v] . prior)
    ([?\C-v] . next)
    ([?\C-d] . ?\C-x)
    ([?\M-d] . (C-S-right delete))
    ([?\M-y] . ?\C-c)
    ([?\M-w] . ?\C-c)
    ([?\C-y] . ?\C-v)
    ([?\C-s] . ?\C-f))
  "Сочетания клавиш для переотправки в X приложения.")

(defconst pro/exwm-browser-classes
  '("firefox" "chromium" "google-chrome" "chromium-browser"
    "brave-browser" "vivaldi-stable")
  "Классы окон, которые считаются браузерами.")

(defmacro exwm-input-set-keys (&rest key-bindings)
  "Установить глобальные EXWM hotkeys.
KEY-BINDINGS - список пар ("клавиша" функция)."
  `(dolist (kb ',key-bindings)
     (cl-destructuring-bind (key cmd) kb
       (exwm-input-set-key (kbd key) cmd))))

(defun pro/exwm-import-display-env ()
  "Передать DISPLAY/XAUTHORITY в systemd --user."
  (let ((display (or (getenv "DISPLAY") ":0"))
        (xauth (or (getenv "XAUTHORITY")
                   (expand-file-name "~/.Xauthority"))))
    (when (fboundp 'pro/log-startup-stage)
      (pro/log-startup-stage "display-env"
                             (format "DISPLAY=%s XAUTHORITY=%s" display xauth)))
    (setenv "DISPLAY" display)
    (setenv "XAUTHORITY" xauth)
    (ignore-errors
      (start-process "systemctl-user-import" nil
                     "systemctl" "--user" "import-environment"
                     "DISPLAY" "XAUTHORITY"))))

(defun pro/exwm-wm-mode-safe ()
  "Запустить `exwm-wm-mode` с диагностикой."
  (message "PRO: вызываю exwm-wm-mode 1 (featurep exwm=%s, fn=%s)"
           (featurep 'exwm) (fboundp 'exwm-wm-mode))
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "exwm-wm" "before exwm-wm-mode"))
  (exwm-wm-mode 1)
  (when (fboundp 'pro/log-startup-stage)
    (pro/log-startup-stage "exwm-wm" "after exwm-wm-mode"))
  (message "PRO: exwm-wm-mode 1 выполнен успешно"))

(defun pro/exwm-load-core ()
  "Загрузить EXWM напрямую и логировать путь к библиотеке."
  (let* ((library (locate-library "exwm"))
         (compiled (and library
                        (let ((candidate (concat (file-name-sans-extension library) ".elc")))
                          (and (file-exists-p candidate) candidate))))
         (source (and library
                      (let ((candidate (concat (file-name-sans-extension library) ".el")))
                        (and (file-exists-p candidate) candidate))))
         (path (or compiled source library)))
    (when (fboundp 'pro/log-startup-stage)
      (pro/log-startup-stage "exwm-load"
                             (format "locate-library=%s source=%s load=%s"
                                     (or library "nil")
                                     (or source "nil")
                                     (or path "nil"))))
    (unless path
      (error "Cannot locate exwm library"))
    (load path nil t)
    (when (fboundp 'pro/log-startup-stage)
      (pro/log-startup-stage "exwm-load" (format "loaded=%s" path)))))

(provide 'про-графическую-среду-ядро)
;;; про-графическую-среду-ядро.el ends here
