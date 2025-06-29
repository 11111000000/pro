;;; пример-early-init.el --- Пример файла раннего запуска -*- lexical-binding: t -*-
;;; Commentary:
;; Этот файл можно скопировать в ~/.emacs.d/early-init.el
;;; Code:
;;;; Оптимизация загрузки пакетного менеджера

;; Отключаем скроллбары для всех фреймов
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(setq-default scroll-bar-mode nil)
(setq-default horizontal-scroll-bar-mode nil)

(setq package-enable-at-startup nil)

;; Предпочитать более новый код

(customize-set-variable 'load-prefer-newer t)

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

;;; пример-early-init.el ends here.
