;;; early-init.el --- Стартовый ранний init для ПРО -*- lexical-binding: t -*-
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      frame-resize-pixelwise t
      package-enable-at-startup nil)
(push '(font . "DejaVu Sans Mono-12") default-frame-alist)
;;; early-init.el ends here
