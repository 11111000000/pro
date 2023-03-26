;;; внешний-вид.el --- Внешний вид и Интерфейс
;; Внешний вид и интерфейс (разделить)
;;; Commentary:
;;; Code:

;;;; Не мигать

(setq visible-bell nil)

;;;; Тулбар

(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

;;;; Меню

(when (bound-and-true-p menu-bar-mode)
  (menu-bar-mode -1))

;;;; Минибуфер

;; Включаем рекурсивные минибуферы

(setq-default enable-recursive-minibuffers nil)

;; Минибуфер - модлайн

(use-package taoline
  :if window-system
  :init (установить-из-репы :repo "11111000000/taoline")
  :custom
  (taoline-show-time t)
  (taoline-show-input t)
  (taoline-show-git-branch t)
  (taoline-show-dir t)
  (taoline-show-previous-buffer  nil)
  :config
  (taoline-mode 1))

;;;; Иконки

(use-package all-the-icons
  :if window-system
  :ensure t)

;;;; Цветовые темы

;; Цветовые темы не должны накладываться друг на друга

(setq-default color-theme-is-cumulative -1)

;; Хук, срабатывающий после установки темы:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;;; Курсор

;; Курсор представляет из себя мигающий прямоугольник, ширина которого зависит от размера символа под ним

(blink-cursor-mode 1)
(setq cursor-type '(bar . 2) )
(setq x-stretch-cursor t)

;; В зависимости от включенного режима ввода, курсор меняет свой вид

(use-package cursor-chg
  :init (установить-из-репы :repo "emacsmirror/cursor-chg")
  :config
  (require 'cursor-chg)
  (change-cursor-mode t)
  (setq curchg-input-method-cursor-color "orange"
        curchg-default-cursor-type '(bar . 2)
        curchg-default-cursor-color (face-attribute 'default :foreground)
        curchg-change-cursor-on-input-method-flag t)

  (add-hook 'after-load-theme-hook
          (lambda ()
            (setq curchg-default-cursor-color (face-attribute 'default :foreground)))))

;;;; Изображения

(use-package image+
  :ensure t
  :after 'image-mode
  :hook (image-mode . image+)
  :bind ((:map image-mode-map
               ("0" . imagex-sticky-restore-original)
               ("+" . imagex-sticky-maximize)
               ("=" . imagex-sticky-zoom-in)
               ("-" . imagex-sticky-zoom-out))))

;;;; Прокрутка

;; Настройки прокрутки

(setq-default scroll-conservatively 101
              scroll-step 0
              scroll-margin 5
              hscroll-step 0
              auto-window-vscroll nil
              hscroll-margin 1)

;; Плавная прокрутка

(when (fboundp 'pixel-scroll-mode)
  (pixel-scroll-mode -1))

;;;; Меню режима

;; Меню для текущего файла, в Org-mode например, показывает список заголовков
;; как своего рода директории

(use-package imenu
  :custom ((imenu-auto-recsan t)))
  :defer t

;;;; Мини-карта

(use-package minimap
  :ensure t
  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background "#222" :foreground "#aaa"))))
  :custom
  (minimap-minimum-width 15)
  (minimap-window-lcoation 'right)
  (minimap-width-fraction 0.08))

;;;; Сокращение диалогов до y/n

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; TODO Путь в заголовке

;;;; Фон для служебных буферов

(use-package solaire-mode
  :ensure t
  :config
  (solaire-global-mode t)
  )

(provide 'внешний-вид)

;;; внешний-вид.el ends here
