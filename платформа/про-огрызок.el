;;; про-огрызок.el --- Настройки для macOS (MacBook) -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: macos, macbook, smooth-scroll, trackpad
;; URL: https://github.com/username/emacs.d/blob/main/платформа/про-огрызок.el
;;
;;; Commentary:
;;
;; Этот файл настраивает специфичные для macOS параметры, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? MacBook имеет особенности: smooth-scroll для
;; плавного скролла, специальные клавиши (Cmd), интеграция с системой.
;; Этот модуль адаптирует Emacs для комфортной работы на Mac.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Плавный скролл
;;  2. Настройки для MacBook
;;  3. Интеграция с macOS
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-огрызок) в init.el
;; только на macOS.
;;
;;; Code:

(modify-all-frames-parameters '((inhibit-double-buffering . t)))

;; (use-package exec-path-from-shell
;;   :defer t 
;;   :ensure t
;;   :init
;;   (exec-path-from-shell-initialize))

(use-package smooth-scroll
  :defer t 
  :ensure t
  :config
  (smooth-scroll-mode -1)
  (setq smooth-scroll/vscroll-step-size 10))

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-set)

(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "C-w") 'delete-window)

(global-set-key (kbd "s-d") 'nil)
(global-set-key (kbd "s-t") 'make-frame)

(global-set-key (kbd "s-q") 'delete-frame)

;; (add-hook 'window-setup-hook (lambda()
;;                                  (setq ns-auto-hide-menu-bar t)
;;                                  (set-frame-position nil 0 -24)
;;                                  (set-frame-size nil (display-pixel-width) (display-pixel-height) t)))

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen) ;; Mac style

(provide 'про-огрызок)
;;; про-огрызок.el ends here
