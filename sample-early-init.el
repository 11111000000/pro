;;; пример-early-init.el --- Пример файла раннего запуска -*- lexical-binding: t -*-
;;; Commentary:
;; Этот файл можно скопировать в ~/.emacs.d/early-init.el
;;; Code:

(defvar pro/early-startup-log-file
  (expand-file-name
   (or (getenv "EMACS_STARTUP_LOG_FILE") "emacs-startup.log")
   (or (getenv "EMACS_STARTUP_LOG_DIR") user-emacs-directory))
  "Файл для очень ранних логов старта Emacs.")

(defun pro/early-log (stage &optional detail)
  "Append early startup STAGE to `pro/early-startup-log-file'."
  (let ((coding-system-for-write 'utf-8)
        (text (format "[%s] EARLY[%s]%s\n"
                      (format-time-string "%F %T%z")
                      stage
                      (if detail (format " %s" detail) ""))))
    (condition-case _err
        (let ((dir (file-name-directory pro/early-startup-log-file)))
          (unless (file-directory-p dir)
            (make-directory dir t))
          (with-temp-buffer
            (insert text)
            (write-region (point-min) (point-max) pro/early-startup-log-file 'append 'silent)))
      (error nil))))

(pro/early-log "start" "sample-early-init.el loaded")
;;;; Оптимизация загрузки пакетного менеджера

;; Отключаем скроллбары для всех фреймов
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(setq-default scroll-bar-mode nil)
(setq-default horizontal-scroll-bar-mode nil)

(setq package-enable-at-startup nil)
(pro/early-log "package" "package-enable-at-startup nil")

;; Предпочитать более новый код

(customize-set-variable 'load-prefer-newer t)
(pro/early-log "custom" "load-prefer-newer t")

;;;; Оптимизация загрузки UI

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . 0) default-frame-alist)
(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
;; Установить шрифт и цвета до создания первого frame
(push '(font . "Aporetic Sans Mono-14") default-frame-alist)
(push '(background-color . "#1e1e1e") default-frame-alist)
(push '(foreground-color . "#d8d8d8") default-frame-alist)
(pro/early-log "frame" "default-frame-alist prepared")

;;; пример-early-init.el ends here.
