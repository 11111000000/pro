;;; управление-окнами.el --- Управление окнами
;; Управление окнами
;;; Commentary:
;;; Code:
;;;; Перемещение по окнам

(global-set-key (kbd "s-h") 'windmove-left)
(global-set-key (kbd "s-j") 'windmove-down)
(global-set-key (kbd "s-k") 'windmove-up)
(global-set-key (kbd "s-l") 'windmove-right)

;;;; Перемещение окон

(leaf buffer-move
  :ensure t  
  :bind (("s-K" . buf-move-up)
         ("s-J" . buf-move-down)
         ("s-H" . buf-move-left)
         ("s-L" . buf-move-right))
  :config)

;;;; Золотое сечение

;; Даёт больше места текущему окну

(leaf golden-ratio
  :ensure t  
  :bind(("C-x +" . golden-ratio)
        ("C-x =" . balance-windows)
        ("C-x _" . maximize-window)
        ("C-x -" . minimize-window))
  :config
  (golden-ratio-mode -1))

;;;; Обзор

;; Позволяет наблюдать на экране все буферы одновременно в уменьшеном виде:

(leaf buffer-expose
  :ensure t
  ;; :load-path "emacs-lisp/buffer-expose"
  :bind (("<s-iso-lefttab>" . buffer-expose)
         (:buffer-expose-grid-map
         ("d" . buffer-expose-kill-buffer)
         ("h" . buffer-expose-left-window)
         ("j" . buffer-expose-down-window)
         ("k" . buffer-expose-up-window)
         ("l" . buffer-expose-right-window)
         ("z" . buffer-expose-ace-window)
         ("RET" . buffer-expose-choose)
         ("SPC" . buffer-expose-choose)
         ("s-SPC" . buffer-expose-choose)
         ("s-<tab>" . buffer-expose-reset)) 
         )
  :init)

;;;; Popwin - предсказуемые попапы

(leaf popwin
  :ensure t
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
  "Toggle popup window by NAME."
  (let ((buf (get-buffer name)))
    (if (get-buffer-window buf t)
        (ignore-errors (delete-window (get-buffer-window buf t)))
      (popwin:pop-to-buffer buf t))))

;; (leaf scratch-pop
;;   :ensure t
;;   :bind (("C-`" . scratch-pop)))

;;;; Переключение окон

(leaf ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
              aw-char-position 'left
              aw-ignore-current nil
              aw-leading-char-style 'char
              aw-scope 'frame)
  :bind (("s-f" . ace-window)
         ("s-F" . ace-swap-window)))

;;;; Показывать буфер с ошибками только при ошибках

(setq warning-minimum-level :error)

(provide 'управление-окнами)
;;; управление-окнами.el ends here
