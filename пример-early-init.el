;;; package --- Summary
;;; Commentary:
;;; Code:

;;; Оптимизация загрузки пакетного менеджера

(setq package-enable-at-startup nil)

;;; Оптимизация загрузки UI

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(set-face-attribute 'default nil :height 160)

(setq inhibit-startup-screen t)
(setq window-resize-pixelwise t)
(setq frame-resize-pixelwise t)
