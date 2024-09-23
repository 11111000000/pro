;;; про-внешний-вид.el --- Внешний вид и Интерфейс  -*- lexical-binding: t -*-
;; Внешний вид и интерфейс (разделить)
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'установить-из)

;;;; Общий вид

;; Система не издаёт лишних звуков и не мигает

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Скрыта панель инструментов и меню

(if window-system
    (tool-bar-mode -1))

(if window-system
    (menu-bar-mode -1))

;; Скрыты полосы прокрутки

(if window-system (scroll-bar-mode -1))

;;;; Минибуфер

;; Минибуферы могут открываться рекурсивно

(setq-default enable-recursive-minibuffers t)

;; Минибуфер может менять размер

(setq-default max-mini-window-height nil)
(setq resize-mini-windows t)
;; (window-resize (minibuffer-window) 0.1)
;; (add-hook 'minibuffer-setup-hook (lambda () (setq line-spacing 1.0)))

;; Длинные сообщения не обрезаются

(setq message-truncate-lines nil)

;; Минибуфер - модлайн

(use-package taoline
  :if window-system
  :after (all-the-icons)
  :functions (taoline-mode)
  :init (установить-из :repo "11111000000/taoline")
  :config
  (taoline-mode 1))

;;;; Иконки
;;;;; All The Icons

(use-package all-the-icons
  :if window-system
  :custom
  (all-the-icons-scale-factor 1)
  (all-the-icons-default-adjust 0)
  :ensure t)

;;;;; Иконки для автодополнения

(use-package kind-icon
  :ensure t
  :after corfu
  :defines (corfu-margin-formatters)
  :functions (kind-icon-margin-formatter kind-icon-reset-cache)
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  ;; (kind-icon-blend-background nil)
  ;; (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'kb/themes-hooks #'(lambda ()
                                (interactive)
                                (kind-icon-reset-cache))))

;; (use-package all-the-icons-completion
;;   :ensure
;;   :after (marginalia all-the-icons)
;;   :functions (all-the-icons-completion-mode)
;;   :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
;;   :init
;;   (all-the-icons-completion-mode))

(use-package nerd-icons-completion
  :ensure t
  :functions (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (unless (display-graphic-p) (nerd-icons-completion-mode)))

;;;;; Иконки Treemacs для Dired

(use-package treemacs-icons-dired
  :ensure t
  :functions (treemacs-icons-dired-mode)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :init
  (add-hook 'after-load-theme-hook
           (lambda ()
             (treemacs-icons-dired-mode -1)
             (sleep-for 0 100)
             (treemacs-icons-dired-mode 1))))

;;;;; Иконки для ibuffer

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; Хук при установке темы:

(defvar after-load-theme-hook nil
  "Хук, срабатывающий после установки темы `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Запускает `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;;; Курсор

;; Курсор представляет из себя мигающий прямоугольник, ширина которого зависит от размера символа под ним:

(setq cursor-type '(bar . 4))

;;(blink-cursor-mode nil)
(setq x-stretch-cursor t)
;;(setq blink-cursor-delay 0.3)

;; В невыбраных окнах, курсор прозрачный

(setq-default cursor-in-non-selected-windows t)

;; В зависимости от включенного режима ввода, курсор меняет свой вид:

(use-package cursor-chg
  :init (установить-из :repo "emacsmirror/cursor-chg")
  :functions (change-cursor-mode)
  :defines (curchg-default-cursor-color)
  :config
  (require 'cursor-chg)
  (setq-default curchg-input-method-cursor-color "orange"
           curchg-default-cursor-type '(bar . 2)
           curchg-default-cursor-color "PaleGreen3" ;(face-attribute 'default :foreground)
           curchg-change-cursor-on-input-method-flag t)
  (change-cursor-mode t))

;;;; Прокрутка

;;;;; Настройки прокрутки

(setq-default scroll-conservatively 80
         scroll-step 1
         scroll-margin 5
         hscroll-step 1
         auto-window-vscroll nil
         fast-but-imprecise-scrolling t
         jit-lock-defer-time 0
         hscroll-margin 1)

;;;;; Плавная прокрутка изображений

(use-package iscroll
  :ensure t
  :functions (iscroll-mode)
  :init
  (add-hook 'org-mode-hook #'iscroll-mode)
  (add-hook 'markdown-mode-hook #'iscroll-mode)
  (add-hook 'image-mode-hook #'iscroll-mode)
  (add-hook 'eww-mode-hook #'iscroll-mode)
  (add-hook 'w3m-mode-hook #'iscroll-mode))

;;;; Меню режима

;; Меню для текущего файла, в Org-mode например, показывает список заголовков
;; как своего рода директории

(use-package imenu
  :custom ((imenu-auto-recsan t))
  :defer t)

;;;; Сокращение диалогов до y/n

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; Фон для служебных буферов

;; (use-package solaire-mode
;;   :ensure t
;;   :config
;;   (solaire-global-mode t))

;;;; Изображения

(require 'image-mode)

(use-package image+
  :ensure t
  :after 'image-mode
  :hook (image-mode . image+)
  :bind ((:map image-mode-map
                 ("0" . imagex-sticky-restore-original)
                 ("+" . imagex-sticky-maximize)
                 ("=" . imagex-sticky-zoom-in)
                 ("-" . imagex-sticky-zoom-out))))

;;;; Подсвечивать при перемещении
(defun pulse-line (&rest _)
  "Pulse the current line."
  (pulse-momentary-highlight-one-line (point)))

(dolist (command '(scroll-up-command
                  scroll-down-command
                  recenter-top-bottom
                  other-window))
  (advice-add command :after #'pulse-line))

;; (defface pulsar-magenta
;;   '((default :extend t)
;;     (((class color) (min-colors 88) (background light))
;;      :background "#ffccff")
;;     (((class color) (min-colors 88) (background dark))
;;      :background "#71206a")
;;     (t :inverse-video t))
;;   "Alternative magenta face for `pulsar-face'."
;;   :group 'pulsar-faces)

;; (use-package pulsar
;;   :ensure t
;;   :defines (pulsar-pulse-functions)
;;   :functions (pulsar-global-mode)
;;   :custom ((pulsar-face 'cursor))
;;   :config
;;   (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
;;   (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
;;   (add-to-list 'pulsar-pulse-functions 'goto-char)
;;   (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
;;   (add-to-list 'pulsar-pulse-functions 'xref-find-definitions-other-window)
;;   :init
;;   (pulsar-global-mode t))

;;;; Подтверждение выключения процессов

(setq-default confirm-kill-processes nil)

;;;; Красивые индикаторы на рамке

(use-package modern-fringes
  :ensure t
  :functions (modern-fringes-mode)
  :init
  (modern-fringes-mode t))

(provide 'про-внешний-вид)
;;; про-внешний-вид.el ends here
