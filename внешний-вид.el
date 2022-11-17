;; *  UI
;; ** Изменение размера шрифта

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C-M-=") 'text-scale-set)


;; ** Сокращение диалогов до y/n

(defalias 'yes-or-no-p 'y-or-n-p)

;; ** Буферы

;; Уникальные имена для буферов

(setq uniquify-buffer-name-style 'forward)

;; Включаем автоактуализацию всех буферов

(global-auto-revert-mode t)
(setq-default global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Клавиша для принудительного обновления

(global-set-key (kbd "C-x C-r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "s-r") (lambda () (interactive) (revert-buffer t t)))

;; Перемещение по окнам

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

(global-set-key (kbd "s-H") 'buf-move-left)
(global-set-key (kbd "s-J") 'buf-move-down)
(global-set-key (kbd "s-K") 'buf-move-up)
(global-set-key (kbd "s-L") 'buf-move-right)

;; Асинхронные буферы скрыты

(add-to-list 'display-buffer-alist
             (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;;  Новые асинхронные буферы переименовываются не спрашивая ничего:

(setq-default async-shell-command-buffer 'rename-buffer)

;; ** Полноэкранный режим

(global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen) ;; Mac style

;; ** Сообщения

(global-set-key (kbd "C-c m") 'popwin:messages)

;; ** Предотвращаем мигание при запуске

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)


;; ** Тулбар скрыт

(when (bound-and-true-p tool-bar-mode)
  (tool-bar-mode -1))

(when (bound-and-true-p menu-bar-mode)
  (menu-bar-mode -1))

;; ** Подсказка комбинаций кавиш

(use-package guide-key
  :ensure t
  :defer t
  :diminish " C-?"
  :custom
  ((guide-key/guide-key-sequence '("C-x" "C-c" "ESC" "C-," "C-z" "C-t" "C-." "s-p"
                                   "M-t" "M-g" "SPC" "C-d" "F1" "M-s"))
   (guide-key/popup-window-position 'top)
   (guide-key/recursive-key-sequence-flag t)
   (guide-key/idle-delay 1.7)
   (guide-key/text-scale-amount -1)
   (guide-key/highlight-prefix-regexp "Prefix")
   (guide-key/highlight-command-regexp
    '("rectangle"
      ("buffer" . "sky blue")
      ("org" . "cornflower blue")
      ("outshine" . "Dark Violet")
      ("helm" . "lime green")
      ("consult" . "lime green")
      ("popwin" . "hot pink")
      ("macro" . "medium orchid")
      ("region" . "cadet blue")
      ("mark" . "moccasin")))
   )
  :init
  (guide-key-mode t))

;; ** Справка

(use-package info   
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

;; ** Документация во всплывающем окне

(use-package eldoc-box  
  :if window-system
  :custom (eldoc-idle-delay 1)
  :hook ((emacs-lisp-mode . eldoc-box-hover-mode)
         (prog-mode . eldoc-box-hover-mode)
         (eglot-managed-mode . eldoc-box-hover-mode)
         (eldoc-box-frame-hook . (lambda () (setq cursor-in-non-selected-windows nil))))
  
  :config
  (setq-default cursor-in-non-selected-windows nil)
  
  (set-face-attribute 'eldoc-box-border nil :background (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'eldoc-box-body nil :family "Iosevka" :weight 'normal :italic nil :height 110)
  )

;; ** Статистика

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

;; ** Минибуфер

;; Когда 7 секунд пользователь ничего не делает, в минибуфере отображается /Системный Монитор/ с часами слева:

(use-package symon
  :disabled t
  :ensure t    
  :if window-system
  :custom
  (symon-delay 12)
  (symon-sparkline-type 'gridded)
  (symon-monitors '(symon-current-time-monitor symon-linux-memory-monitor symon-linux-cpu-monitor symon-linux-battery-monitor))
  :init
  (symon-mode) 
  (symon-display)
)

;; Размер шрифта в минибуфере
;; TODO конфликт с taoline ?
;; (dolist
;;     (buf (list " *Minibuf-0*" " *Minibuf-1*" " *Echo Area 0*" " *Echo Area 1*" "*Quail Completions*"))
;;   (when (get-buffer buf)
;;     (with-current-buffer buf
;;       (setq-local face-remapping-alist '((default (:height 1.5)))))))

;; Минибуфер во фрейме поверх окна

;; (use-package mini-frame
;;   :ensure t
;;   :config (custom-set-variables
;;            '(mini-frame-show-parameters
;;              '(
;;                (child-frame-border-width . 1)
;;                (internal-border-width . 1)
;;                (top . 0)
;;                (width . 1.0)
;;                (left . 0)
;;                )

;;              )
;;            '(mini-frame-standalone t)
;;            )
;;   :init
;;   (mini-frame-mode t)
;;   )


;; Чтобы избежать путаницы, иногда случайно открывая /Минибуфер/ внутри /Минибуфера/, выключаем /Рекурсивные/ /Минибуферы/ 

(setq-default enable-recursive-minibuffers nil)

;; ** Показать текущее время

(global-set-key (kbd "s-<f1>") (lambda () (interactive) (print (current-time-string))))

;; ** Скроллбар

(if window-system (scroll-bar-mode -1))

;; ** Иконки

;; Во первых нужны сами иконочные шрифты, поместим их в ~/.locale/share/fonts :

;; > mkdir -p ~/.local/share/fonts/ ~/tmp/
;; > git clone git@github.com:domtronn/all-the-icons.el.git ~/tmp/all-the-icons
;; > cp ~/tmp/all-the-icons/fonts/* ~/.local/share/fonts

(use-package all-the-icons
  ;;:if window-system
  :ensure t
  )

;; ** Цвет

;; Цветовые темы не должны накладываться друг на друга

(setq-default color-theme-is-cumulative -1)

;; Хук, срабатывающий после установки темы:

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; ** Изображения

(use-package image+
  :ensure t  
  :after 'image-mode
  :hook (image-mode . image+)
  :bind ((:map image-mode-map
               ("0" . imagex-sticky-restore-original)
               ("+" . imagex-sticky-maximize)
               ("=" . imagex-sticky-zoom-in)
               ("-" . imagex-sticky-zoom-out))))
;; ** Прокрутка

;; Настройки прокрутки

(setq-default scroll-conservatively 101
              scroll-step 0
              scroll-margin 5
              hscroll-step 0
              auto-window-vscroll nil
              hscroll-margin 1)

;; Плавная прокрутка

                                        ; (when (fboundp 'pixel-scroll-mode)
                                        ;   (pixel-scroll-mode 1))

;; Сохранение расположения

(use-package eyebrowse
  :ensure t
  :config (eyebrowse-mode))


;; ** Меню для буфера

;; Меню для режима текущего файла, например в Org-mode показывает список заголовков как своего рода директории

(use-package imenu
  :defer t
  :custom ((imenu-auto-rescan t))  
  )


;; ** Перемещение окон

(use-package buffer-move
  :ensure t
  :defer t
  :bind (("s-K" . buf-move-up)
         ("s-J" . buf-move-down)
         ("s-H" . buf-move-left)
         ("s-L" . buf-move-right))
  :config
  )

;; ** Золотое сечение

;; Даёт больше места текущему окну:

(use-package golden-ratio
  :ensure t
  :defer t
  :bind(("C-x +" . golden-ratio)
        ("C-x =" . balance-windows)
        ("C-x _" . maximize-window)
        ("C-x -" . minimize-window))
  :config
  (golden-ratio-mode -1))

;; ** Обзор

;; Позволяет наблюдать на экране все буферы одновременно в уменьшеном виде:

(use-package buffer-expose
  :ensure t
  ;; :load-path "emacs-lisp/buffer-expose"
  :bind (("<s-iso-lefttab>" . buffer-expose)
         :map buffer-expose-grid-map
         ("d" . buffer-expose-kill-buffer)
         ("h" . buffer-expose-left-window)
         ("j" . buffer-expose-down-window)
         ("k" . buffer-expose-up-window)
         ("l" . buffer-expose-right-window)
         ("z" . buffer-expose-ace-window)
         ("RET" . buffer-expose-choose)
         ("SPC" . buffer-expose-choose)
         ("s-SPC" . buffer-expose-choose)
         ("s-<tab>" . buffer-expose-reset)
         ) 
  :init  
  )

  ;; (use-package time
  ;;   :custom
  ;;   (display-time-24hr-format t)
  ;;   (display-time-use-mail-icon t)
  ;;   (display-time-day-and-date nil)
  ;;   (display-time-world-list '(("Asia/Irkutsk" "IRK")
  ;; 			     ("Europe/Moscow" "MSK")))
  ;;   :config
  ;;   (display-time-mode))

  ;; (use-package dashboard  
  ;;   :requires all-the-icons
  ;;   :ensure t
  ;;   :custom
  ;;   (dashboard-banner-logo-title "DOBRO")
  ;;   (dashboard-startup-banner 'logo)
  ;;   (dashboard-center-content t)
  ;;   (dashboard-show-shortcuts nil)
  ;;   (dashboard-items '((recents . 5)
  ;;   		     (bookmarks . 5)
  ;;   		     (agenda . 5)
  ;;   		     (registers . 5)))
  ;;   (dashboard-set-heading-icons t)
  ;;   (dashboard-set-file-icons t)
  ;;   (dashboard-set-navigator t)
  ;;   (dashboard-set-init-info t)
  ;;   (dashboard-set-footer nil)
  ;;   (show-week-agenda-p t)
  ;;   (initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  ;;   :config
  ;;   (dashboard-setup-startup-hook))

  ;; (use-package fancy-battery
  ;;   :ensure t
  ;;   :config
  ;;     (setq fancy-battery-show-percentage t)
  ;;     (setq battery-update-interval 15)
  ;;     (if window-system
  ;;       (fancy-battery-mode)
  ;;       (display-battery-mode)))

  ;; (use-package smart-mode-line  
  ;;   :ensure t
  ;;   :custom
  ;;   (sml/no-confirm-load-theme t)
  ;;   (sml/theme 'respectful)
  ;;   :config
  ;;   (sml/setup))

;; ** Popwin - предсказуемые попапы

(use-package popwin
  :ensure t
  :defer t
  :bind (
         ("C-c b" . popwin:popup-buffer)    
         ("C-c ." . popwin:stick-popup-window)
         )
  :config

  (setq popwin:special-display-config
        '(("*Miniedit Help*" :noselect t)
          help-mode
          lsp-ui-imenu-mode
          treemacs-mode
          special-mode
          telega-chat-mode
          (completion-list-mode :noselect t)
          (compilation-mode :noselect t)
          (grep-mode :noselect t)
          (occur-mode :noselect t)
          (Man-mode :noselect nil :position top)
          ("*Pp Macroexpand Output*" :noselect t)
          "*Shell Command Output*"
          "*Backtrace*"
          "*vc-diff*"
          "*vc-change-log*"
          (" *undo-tree*" :width 60 :position right)
          ("^.*Developer.*$" :regexp t :width .5 :position right)
          ("^\\*anything.*\\*$" :regexp t)
          "*slime-apropos*"
          "*slime-macroexpansion*"
          "*slime-description*"
          ("*slime-compilation*" :noselect t)
          "*slime-xref*"
          "*Calendar*"
          ("*Messages*" :noselect t :position bottom :stick t)
          ("*Racket Describe*" :noselect t :position top :stick t)
          ("*Racket REPL*" :noselect t :position bottom :stick t)
          ("*prettier errors*" :noselect t :position top :stick nil)
          ("Run.rkt" :noselect t :position right :width .5 :stick t)
          ("chrome dev" :noselect nil :position bottom :stick t :height .5)
          ("^\\*Launch.*$" :regexp t :noselect nil :position bottom :stick t :height .5)
          ("chrome dev2" :noselect t :position top :height 15 :stick t)                  
          ("gimp" :regexp t :noselect nil :position right :width .5 :stick nil)
          ("chrome app" :noselect nil :position right :stick t :width .5)                  
          ("ff dev" :noselect nil :position bottom :height .5 :stick t)
          ("ff" :noselect nil :position right :width .5 :stick t)
          
          (sldb-mode :stick t)                  
          ;;(shell-mode :stick nil :position bottom )
          ))
  (popwin-mode 1)
  ;; (push '(:regexp :position top) popwin:special-display-config)
  )

;; Функция для переключения окна в попапе по имени буфера

(defun om/popwin-toggle-name (name)
  "Toggle popup window by name"
  (let ((buf (get-buffer name)))
    (if (get-buffer-window buf t)
        (ignore-errors (delete-window (get-buffer-window buf t)))
      (popwin:pop-to-buffer buf t))))

;; (use-package scratch-pop
;;   :ensure t
;;   :defer t
;;   :bind (("C-`" . scratch-pop)))

;; Минибуфер - модлайн

(use-package taoline
  :straight '(taoline :host github :repo "11111000000/taoline")
  :if window-system
  :custom
  (taoline-show-time t)
  (taoline-show-input t)
  (taoline-show-git-branch t)
  (taoline-show-dir t)
  (taoline-show-previous-buffer  nil)
  :config
  (taoline-mode 1))

;; (use-package doom-modeline
;;   :ensure t
;;   :custom ((doom-modeline-height 8))
;;   :config
;;   (custom-set-faces
;;    '(mode-line ((t (:family "Iosevka" :height 140))))
;;    '(mode-line-inactive ((t (:family "Iosevka" :height 140)))))
;;   :init 
;;   (doom-modeline-mode t))


;; Конец файла

;; Мини-карта

(use-package minimap 
  :ensure t
  :config
  (custom-set-faces
   '(minimap-active-region-background ((t :background "#222" :foreground "#aaa"))
                                      ))
  :custom 
  (minimap-window-location 'right)
  
  (defun my/toggle-minimap ()
    "Toggle minimap for current buffer."
    (interactive)
    (if (null minimap-bufname)
        (minimap-create)
      (minimap-kill)))
  
  )

;; ** Переключение окон

(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("s-f" . ace-window)
         ("s-F" . ace-swap-window)))


;; ** TODO Путь в заголовке



(provide 'внешний-вид)
