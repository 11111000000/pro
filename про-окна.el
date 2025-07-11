;;; про-окна.el --- Управление окнами -*- lexical-binding: t -*-
;; Управление окнами
;;; Commentary:
;;; Code:

;;;; Перемещение окон

(use-package buffer-move
  :ensure t
  :defer t
  :config)

;;;; Золотое сечение

;; Даёт больше места текущему окну

(use-package golden-ratio
  :ensure t
  :defer t
  :config
  (golden-ratio-mode -1))

;;;; Обзор

;; Позволяет наблюдать на экране все буферы одновременно в уменьшеном виде:

(use-package buffer-expose
  :ensure t
  ;; :load-path "emacs-lisp/buffer-expose"
  :bind (
         :map buffer-expose-grid-map
         ("d" . buffer-expose-kill-buffer)
         ("h" . buffer-expose-left-window)
         ("j" . buffer-expose-down-window)
         ("k" . buffer-expose-up-window)
         ("l" . buffer-expose-right-window)
         ("z" . buffer-expose-ace-window)
         ("RET" . buffer-expose-choose)
         ("SPC" . buffer-expose-choose)
         ("s-o" . buffer-expose-reset))
  :init)

;;;; Popper - предсказуемые попапы

(use-package popper
  :ensure t
  :after (projectile)
  :custom (popper-window-height 20)
  :bind (("s-,"   . popper-toggle)
         ("s-]"   . nil)
         ("s-["   . nil)
         ("s-/" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-projectile)
  (setq popper-reference-buffers
       '("\\*Messages\\*"
         "Output\\*$"
         shell-command-mode
         help-mode
         compilation-mode
         calendar-mode
         chatgpt-shell-mode
         ;; "^\\*aidermacs.*\\*$" aidermacs-comint-mode
         ;;"^\\*eshell.*\\*$" eshell-mode
         ;;"^\\*shell.*\\*$"  shell-mode
         "^\\*term.*\\*$"   term-mode
         "^\\*vterm.*\\*$"  vterm-mode
         ))
  (popper-mode +1)
  (popper-echo-mode +1)
  (add-to-list 'display-buffer-alist
             (cons "\\*Async.*" (cons #'display-buffer-no-window nil)))
  )

;; (use-package popwin
;;   :ensure t
;;   :config
;;   (require 'popwin)
;;   (setq popwin:special-display-config
;;         '(("*Miniedit Help*" :noselect t)
;;           lsp-ui-imenu-mode
;;           special-mode
;;           (completion-list-mode :noselect t)
;;           (compilation-mode :noselect t)
;;           (grep-mode :noselect t)
;;           (occur-mode :noselect t)
;;           (Man-mode :noselect nil :position top)
;;           ("*Pp Macroexpand Output*" :noselect t)
;;           "*Shell Command Output*"
;;           "*Backtrace*"
;;           (chatgpt-shell-mode :position bottom :stick t)
;;           "*vc-diff*"
;;           "*vc-change-log*"
;;           (" *undo-tree*" :width 60 :position right)
;;           ("^.*Developer.*$" :regexp t :width .5 :position right)
;;           ("^\\*anything.*\\*$" :regexp t)
;;           "*slime-apropos*"
;;           "*slime-macroexpansion*"
;;           "*slime-description*"
;;           ("*slime-compilation*" :noselect t)
;;           "*slime-xref*"
;;           "*Calendar*"
;;                                         ;(vterm-mode :noselect t :position bottom :stick t)
;;           ("*Messages*" :noselect t :position bottom :stick t)
;;           ("*Warnings*" :noselect t :position bottom :stick t)
;;           ("*Racket Describe*" :noselect t :position top :stick t)
;;           ("*Racket REPL*" :noselect t :position bottom :stick t)
;;           ("*ielm*" :noselect t :position bottom :stick t)
;;           ("*prettier errors*" :noselect t :position top :stick nil)
;;           ("Run.rkt" :noselect t :position right :width .5 :stick t)
;;           ("chrome dev" :noselect nil :position bottom :stick t :height .5)
;;           ("^\\*Launch.*$" :regexp t :noselect nil :position bottom :stick t :height .5)
;;           ("chrome dev2" :noselect t :position top :height 15 :stick t)
;;           ("gimp" :regexp t :noselect nil :position right :width .5 :stick nil)
;;           ("chrome app" :noselect nil :position right :stick t :width .5)
;;           ("ff dev" :noselect nil :position bottom :height .5 :stick t)
;;           ("ff" :noselect nil :position right :width .5 :stick t)
;;           ;;(shell-mode :stick nil :position bottom )
;;           (sldb-mode :stick t)))
;;   (popwin-mode 1))

;; Функция для переключения окна в попапе по имени буфера

;;(require 'popwin)

;; (defun показать-окно-в-попапе (name)
;;   "Toggle popup window by NAME."
;;   (let ((buf (get-buffer name)))
;;     (if (get-buffer-window buf t)
;;         (ignore-errors (delete-window (get-buffer-window buf t)))
;;       (popwin:pop-to-buffer buf t))))

;;;; Переключение окон

(use-package ace-window
  :ensure t
  :init (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
            aw-char-position 'left
            aw-ignore-current nil
            aw-leading-char-style 'char
            aw-scope 'frame))

(provide 'про-окна)
;;; про-окна.el ends here
