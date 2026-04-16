;;; про-справку.el --- Справка и подсказки в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: help, helpful, which-key, info, elisp-demos
;; URL: https://github.com/username/emacs.d/blob/main/интерфейс/про-справку.el
;;
;;; Commentary:
;;
;; Этот файл настраивает справку и подсказки в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Справка — ключ к продуктивности в Emacs. Здесь мы
;; настраиваем улучшенную справку (helpful), демо для Elisp, подсказки
;; клавиш (which-key) и навигацию по Info. Это делает изучение Emacs
;; более приятным и быстрым.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Info и навигация
;;  2. Helpful для документации
;;  3. Which-key для подсказок
;;  4. Elisp demos
;;  5. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-справку) в init.el.
;; Рекомендуется подключать после про-быстрый-доступ.
;;
;;; Code:

;;;; Info

(use-package info
  :defer t
  :bind (:map Info-mode-map
                ("DEL" . Info-history-back)
                ("B" . Info-history-back)
                ("F" . Info-history-forward)
                ("h" . Info-up)
                ("j" . next-line)
                ("k" . previous-line)
                ("l" . Info-follow-nearest-node)
                ("<XF86Back>" . nil)
                ("<XF86Forward>" . nil)))

;;;; Дополнительная справка

(use-package helpful
  :ensure t
  :defer t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-h ." . helpful-at-point))
  

;;;; Изучение API Elisp

(use-package elisp-demos
  :defer t
  :ensure t
  :functions (elisp-demos-advice-helpful-update)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Подсказка комбинаций кавиш

(use-package which-key
  :ensure t
  :defer t
  :diminish which-key-mode
  :custom
  ((which-key-idle-delay 3)
   (which-key-max-description-length 40)
   (which-key-sort-order 'which-key-prefix-then-key-order)
   (which-key-show-prefix 'top))
  :init
  (which-key-mode 1))

(use-package which-key-posframe
  :ensure t
  :after which-key
  :if (display-graphic-p)
  :custom
  (which-key-posframe-border-width 3)
  (which-key-posframe-poshandler 'posframe-poshandler-frame-bottom-center)
  (which-key-posframe-parameters '((left-fringe . 6) (right-fringe . 6)))
  :hook (which-key-mode . which-key-posframe-mode))

;;;; Документация по языку во всплывающем окне

(use-package eldoc-box
  :ensure t
  :if (display-graphic-p)
  :functions (eldoc-box-hover-mode)
  :bind (("M-/" . eldoc-box-help-at-point)) ;; TODO: Перенести в org
  :custom ((eldoc-idle-delay 0.2)
          (eldoc-box-offset '(-40 50 -70))
          (eldoc-idle-delay 1.0))
  :config
  (require 'eldoc)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-box-hover-mode t))

;;;; Статистика нажатий

(use-package keyfreq
  :defer t
  :ensure t
  :if (display-graphic-p)
  :functions (keyfreq-mode keyfreq-autosave-mode)
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode))

;; В TTY не включаем hover-posframe-стиль подсказок: они часто вызывают
;; лишнюю перерисовку и визуальный шум при вводе.


(provide 'про-справку)
;;; про-справку.el ends here
