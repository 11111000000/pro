;;; внешний-вид.el --- Внешний вид и Интерфейс
;; Внешний вид и интерфейс (разделить)
;;; Commentary:
;;; Code:

;;;; Не мигать
(setq visible-bell nil)

;;;; Буферы

;; Уникальные имена для буферов

(setq uniquify-buffer-name-style 'forward)

;; Включаем автоактуализацию всех буферов

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq-default auto-revert-verbose nil)

;; Обновление буфера без вопросов

(defun обновить-буфер-немедленно ()
  "ничего не спрашивая, обновить буфер."
  (interactive)
  (revert-buffer t t))

;; Асинхронные буферы скрыты

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; Новые асинхронные буферы переименовываются не спрашивая ничего:

(setq-default async-shell-command-buffer 'rename-buffer)

;; Буфер с ошибками только при ошибках

(setq warning-minimum-level :error)

;; Скроллбар

(if window-system (scroll-bar-mode -1))

;;;; Тулбар

(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

;;;; Меню

(when (bound-and-true-p menu-bar-mode)
  (menu-bar-mode -1))

;;;; Минибуфер

;; Включаем рекурсивные минибуферы

(setq-default enable-recursive-minibuffers t)

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
(setq cursor-type '(bar . 3) )
(setq x-stretch-cursor t)

;; В зависимости от включенного режима ввода, курсор меняет свой вид

(use-package cursor-chg
  :init (установить-из-репы :repo "emacsmirror/cursor-chg")
  :config
  (require 'cursor-chg)
  (change-cursor-mode t)
  (setq curchg-input-method-cursor-color "orange"
        curchg-default-cursor-type 'bar
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

;;;;  Вкладки

;;;;;  Верхний уровень вкладок

(use-package tab-bar
  :ensure t
  :config
  (tab-bar-mode t)
  (setq-default tab-bar-close-button nil)
  (dotimes (i 10)
    (global-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i))))
  (defvar список-кружков-с-цифрами
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")
  
  (defun формат-имени-вкладки (tab i)
    (let ((current-p (eq (car tab) 'current-tab))
         (tab-num (if (and tab-bar-tab-hints (< i 10))
                      (alist-get i список-кружков-с-цифрами) "")))
      (propertize
       (concat tab-num
              " "
              (alist-get 'name tab)
              (or (and tab-bar-close-button-show
                     (not (eq tab-bar-close-button-show
                              (if current-p 'non-selected 'selected)))
                     tab-bar-close-button)
                  "")
              " ")
       'face (funcall tab-bar-tab-face-function tab))))
  
  (setq tab-bar-tab-name-format-function #'формат-имени-вкладки)
  (setq tab-bar-tab-hints t))

;;;;;  Вкладки уровня окна

(use-package tab-line
  :config
  (global-tab-line-mode t)

  ;;The default tabline is ugly, it also shows the button to create or delete a tab. To disable those button, add the following config:

  (setq tab-line-new-button-show nil)  ;; do not show add-new button
  (setq tab-line-close-button-show nil)  ;; do not show close button

  ;;To change the separator between tabs, set the variable tab-line-separator:

  (setq tab-line-separator "")  ;; set it to empty

  ;;To further customize the look of tabs, we can employ the powerline package:
  (require 'powerline)
  (defvar az/tab-height 22)
  (defvar az/tab-left (powerline-wave-right 'tab-line nil az/tab-height))
  (defvar az/tab-right (powerline-wave-left nil 'tab-line az/tab-height))

  (defun az/tab-line-tab-name-buffer (buffer &optional _buffers)
    (powerline-render (list az/tab-left
                            (format "%s" (buffer-name buffer))
                            az/tab-right)))
  (setq tab-line-tab-name-function #'az/tab-line-tab-name-buffer)

  ;; tab color settings
  ;; (set-face-attribute 'tab-line nil :height 1.0)
  ;; (set-face-attribute 'tab-line-tab nil :height 1.0 :inherit 'tab-line)
  ;; (set-face-attribute 'tab-line-tab-current nil :height 1.0)
  ;; (set-face-attribute 'tab-line-tab-inactive nil :height 1.0)
  ;; (set-face-attribute 'tab-line-highlight nil :height 1.0)
  )

;; (use-package tabbar
;;   :ensure t
;;   :hook ((eldoc-box-frame-hook . tabbar-local-mode))
;;   :custom
;;   (tabbar-buffer-groups-function 'dobro/buffer-groups)
;;   (tabbar-cycle-scope 'tabs)
;;   :config
;;   ;; ЧТОДЕЛ: Функция для переключение этих вкладок по номеру нужна
;;   ;; (dotimes (i 10)
;;   ;;   (global-set-key (kbd (format "C-s-%d" i)) `(lambda () (interactive) (tabbar-select-tab ,i))))i
  
;;   (require 'memoize)
  
;;   ;; (defmemoize сгруппировано-по-проекту1 ()
;;   ;;   (list
;;   ;;    (cond
;;   ;;     (
;;   ;;      (memq major-mode '(mu4e-view-mode mu4e-main-mode mu4e-headers-mode mu4e-view-raw-mode
;;   ;;                                        twittering-mode weibo-timeline-mode telega-mode telega-chat-mode telega-root-mode
;;   ;;                                        jabber-roster-mode jabber-chat-mode erc-mode douban-music-mode))
;;   ;;      "Activity")
;;   ;;     ((memq major-mode '(eww-mode))
;;   ;;      "EWW")
;;   ;;     ((memq major-mode '(exwm-mode))
;;   ;;      "Xorg")
;;   ;;     ((memq major-mode '(term-mode vterm-mode shell-mode))
;;   ;;      "Terminals")
;;   ;;     ((string-equal "*" (substring (buffer-name) 0 1))
;;   ;;      "Emacs")
;;   ;;     ((memq major-mode '(fundamental-mode))
;;   ;;      "Emacs")
;;   ;;     ;; (
;;   ;;     ;;  ;; (memq (current-buffer)
;;   ;;     ;;  ;;       (condition-case nil
;;   ;;     ;;  ;;           (projectile-buffers-with-file-or-process (projectile-project-buffers))
;;   ;;     ;;  ;;         (error nil)))
;;   ;;     ;; ((memq major-mode '(org-mode org-agenda-mode diary-mode))
;;   ;;     ;;  "OrgMode"
;;   ;;     ;;  )
;;   ;;     (t
;;   ;;      (or (projectile-project-name) "Common")
;;   ;;      ))))

;;   ;; (defun my-make-throttler ()
;;   ;;   (let ((last-time (float-time))
;;   ;;        (last-res ()))
;;   ;;     (lambda (&rest args)
;;   ;;       (if (< 1 (- (float-time) last-time))
;;   ;;           last-res
;;   ;;         (setq last-time (float-time))
;;   ;;         (setq last-res (apply args))))))
;;   ;;(advice-add 'сгруппировано-по-проекту :override (my-make-throttler))

;;   (setq tabbar-buffer-groups-function 'сгруппировано-по-проекту1)

;;   (tabbar-mode -1)
;;   (tabbar-mode 1))

;; (use-package project-tab-groups
;;   :ensure
;;   :config
;;   (project-tab-groups-mode 1))

(defun закрыть-вкладку-и-буфер ()
  "Закрывает вкладку и буфер в ней."
  (interactive)
  (kill-this-buffer)
  (tab-close)
  )

(provide 'внешний-вид)

;;; внешний-вид.el ends here
