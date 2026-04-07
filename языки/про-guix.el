;;; про-guix.el --- Поддержка Guix в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: guix, geiser, guile, scheme, package-manager
;; URL: https://github.com/username/emacs.d/blob/main/языки/про-guix.el
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку Guix в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Guix — мощный менеджер пакетов на основе Nix,
;; с поддержкой воспроизводимых окружений. Здесь мы загружаем пакеты
;; из .guix-profile и настраиваем geiser-guile для разработки на Guile.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Загрузка пакетов из Guix
;;  2. Настройка Geiser Guile
;;  3. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-guix) в init.el.
;; Требует установленного Guix.
;;
;;; Code:
;;; Пакеты Guix

(if (file-exists-p "/home/az/.guix-profile/share/emacs/site-lisp")
    (add-to-list 'load-path "/home/az/.guix-profile/share/emacs/site-lisp")
  (guix-emacs-autoload-packages))

;;; Geiser Guile

(use-package geiser-guile
  :ensure t
  :defines (geiser-guile-load-path)
  :config
  ;; (add-to-list 'geiser-guile-load-path "~/System/channels/nonguix")
  (add-to-list 'geiser-guile-load-path "~/System/channels/chan"))

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/System/channels/guix/etc/snippets"))

(provide 'про-поддержку-guix)
;;; про-поддержка-guix.el ends here
