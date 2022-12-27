;;; пример-early-init.el --- Пример файла раннего запуска
;;; Commentary:
;; Этот файл можно скопировать в ~/.emacs.d/early-init.el
;;; Code:
;;;; Оптимизация загрузки пакетного менеджера

(setq package-enable-at-startup nil)

;;;; Оптимизация загрузки UI

;; (push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
;;; пример-early-init.el ends here.
