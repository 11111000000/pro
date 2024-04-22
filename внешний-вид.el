;;; внешний-вид.el --- Внешний вид и Интерфейс  -*- lexical-binding: t -*-
;; Внешний вид и интерфейс (разделить)
;;; Commentary:
;;; Code:

(require 'use-package)

;;;; Общий вид

;; Система не издаёт лишних звуков и не мигает

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Скрыта панель инструментов и меню

(when (bound-and-true-p tool-bar-mode)
    (tool-bar-mode -1))

(when (bound-and-true-p menu-bar-mode)
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

;; Шрифт минибуфера

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defun my-minibuffer-setup ()
;;        (set (make-local-variable 'face-remapping-alist)
;;             '((default :height 1.2))))

;; (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
;;   (with-current-buffer (get-buffer buf)
;;     (make-local-variable 'face-remapping-alist)
;;     (add-to-list 'face-remapping-alist '(default (:background "white" :height 1.3)))))

;; Минибуфер - модлайн

(use-package taoline
  :if window-system
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

;;;;; Иконки для режимов
;; TODO: Настроить с taoline

;; (use-package mode-icons
;;   :ensure t
;;   :custom (
;;           (mode-icons-desaturate-active t))
;;   :config
;;   ;; В новых версиях Emacs есть два режима ELisp\d и ELisp\l - для динамической и лексической области видимости соотв.
;;   (add-to-list 'mode-icons  '(".*ELisp.*" "emacs" xpm))
;;   (add-to-list 'mode-icons  '(".*EXWM.*" #xf1b2 FontAwesome))
;;   (add-to-list 'mode-icons  '(".*Dired.*" #xf07c FontAwesome))
;;   (add-to-list 'mode-icons  '(".*TypeScript.*" #xeb1b FontAwesome))
;;   (add-to-list 'mode-icons  '(".*HTML.*" "html" xpm))
;;   (add-to-list 'mode-icons  '(".*Less.*" #xf1c9 FontAwesome))
;;   (mode-icons-mode t))

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

(use-package all-the-icons-completion
  :ensure
  :after (marginalia all-the-icons)
  :functions (all-the-icons-completion-mode)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package nerd-icons-completion
  :ensure t
  :functions (nerd-icons-completion-mode)
  :config
  (unless (display-graphic-p) (nerd-icons-completion-mode)))





;;;;; Иконки Treemacs для Dired

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :init
  (add-hook 'after-load-theme-hook
           (lambda ()
             (treemacs-icons-dired-mode -1)
             (sleep-for 0 100)
             (treemacs-icons-dired-mode 1))))


;;;; Хук при установке темы:

(defvar after-load-theme-hook nil
  "Хук, срабатывающий после установки темы `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Запускает `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;;; Курсор

;; В 1968 году американский изобретатель Дуглас Карл Энгельбарт, совместно с командой из Стэнфордского исследовательского института, создают первые элементы человеко-машинного интерфейса (UI — англ. user interface). Эти элементы являлись частью революционной разработки NLS (NLS — англ. oN-Line System) системы которая реализовывала одновременную работу нескольких терминалов, над одними и теми же документами. В этом же году 9 декабря, состоялась демонстрация онлайн системы, которая получила название «The Mother of All Demos» дословно «мать всех демонстраций», на которой можно было увидеть работу с гипертекстом и курсор.

;; Курсор представляет из себя мигающий прямоугольник, ширина которого зависит от размера символа под ним:

                                        ;(blink-cursor-mode nil)
(setq cursor-type '(bar . 2))
                                        ;(setq x-stretch-cursor 1)
                                        ;(setq blink-cursor-delay 0.3)

;; В невыбраных окнах, курсор прозрачный

                                        ;(setq-default cursor-in-non-selected-windows nil)

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

  (change-cursor-mode t)
  ;; (add-hook 'after-load-theme-hook
  ;;          (lambda ()
  ;;            (change-cursor-mode t)
  ;;                                       ;(setq curchg-default-cursor-color (face-attribute 'default :foreground))
  ;;            ))
  )

;;;; Прокрутка

;; Настройки прокрутки

(setq-default scroll-conservatively 101
         scroll-step 0
         scroll-margin 5
         hscroll-step 0
         auto-window-vscroll nil
         fast-but-imprecise-scrolling t
         jit-lock-defer-time 0
         hscroll-margin 1)

;;;; Быстрая прокрутка

;; (require 'flycheck)

;; (use-package fast-scroll
;;   :defer 1
;;   :hook
;;   (fast-scroll-start . (lambda () (flycheck-mode -1)))
;;   (fast-scroll-end . (lambda () (flycheck-mode 1)))
;;   :config
;;   (fast-scroll-config)
;;   (fast-scroll-mode 1))

;; Плавная прокрутка

;; (when (fboundp 'pixel-scroll-mode)
;;   (pixel-scroll-mode t))

;;;; Меню режима

;; Меню для текущего файла, в Org-mode например, показывает список заголовков
;; как своего рода директории

(use-package imenu
  :custom ((imenu-auto-recsan t))
  :defer t)

;;;; Мини-карта

(use-package minimap
  :ensure t
  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background "#111" :foreground "#aaa"))))
  :custom
  (minimap-minimum-width 14)
  (minimap-window-location 'right)
  (minimap-width-fraction 0.08))

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

(defface pulsar-magenta
  '((default :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffccff")
    (((class color) (min-colors 88) (background dark))
     :background "#71206a")
    (t :inverse-video t))
  "Alternative magenta face for `pulsar-face'."
  :group 'pulsar-faces)


(use-package pulsar
  :ensure t
  :defines (pulsar-pulse-functions pulsar-global-mode)
  :custom ((pulsar-face 'cursor))
  :config
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'goto-char)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions)
  (add-to-list 'pulsar-pulse-functions 'xref-find-definitions-other-window)
  :init
  (pulsar-global-mode t))

;; TODO Рамки окон...
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;; 		      window-divider-first-pixel
;; 		      window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))


;;;; Подтверждение выключения процессов

(setq-default confirm-kill-processes nil)

(provide 'внешний-вид)
;;; внешний-вид.el ends here
