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

;; (use-package popper
;;   :ensure t
;;   :after (projectile)
;;   :custom
;;   (popper-window-height 20)
;;   (popper-display-control nil)
;;   :bind (("s-,"   . popper-toggle)
;;          ("s-]"   . nil)
;;          ("s-["   . nil)
;;          ("s-/" . popper-toggle-type))
;;   :init
;;   (setq popper-group-function #'popper-group-by-projectile)
;;   (setq popper-reference-buffers
;;         '("\\*Messages\\*"
;;           "Output\\*$"
;;           shell-command-mode
;;           help-mode
;;           compilation-mode
;;           calendar-mode
;;           chatgpt-shell-mode
;;           ;; "^\\*aidermacs.*\\*$" aidermacs-comint-mode
;;           ;;"^\\*eshell.*\\*$" eshell-mode
;;           ;;"^\\*shell.*\\*$"  shell-mode
;;           "^\\*term.*\\*$"
;;           term-mode
;;           "^\\*vterm.*\\*$"
;;           "^\\*gptel.*\\*$"
;;           "^\\*AI.*\\*$"
;;           vterm-mode
;;           gptel-aibo-mode
;;           gptel-mode
;;           "^\\*nixos-log\\*$"))
;;   (popper-mode +1)
;;   ;; GPTel: открывать справа (и по имени, и по major-mode)
;;   (add-to-list 'display-buffer-alist
;;                '( (lambda (buf _action)
;;                     (with-current-buffer buf
;;                       (or (derived-mode-p 'gptel-mode 'gptel-aibo-mode)
;;                           (string-match-p "^\\*gptel.*\\*$" (buffer-name)))))
;;                   (display-buffer-reuse-window display-buffer-in-side-window)
;;                   (side . right)
;;                   (slot . 1)
;;                   (window-width . 0.35)
;;                   (window-parameters . ((no-other-window . nil)))))
;;   ;; vterm: открывать снизу (и по имени, и по major-mode)
;;   (add-to-list 'display-buffer-alist
;;                '( (lambda (buf _action)
;;                     (with-current-buffer buf
;;                       (or (derived-mode-p 'vterm-mode)
;;                           (string-match-p "^\\*vterm.*\\*$" (buffer-name)))))
;;                   (display-buffer-reuse-window display-buffer-in-side-window)
;;                   (side . bottom)
;;                   (slot . 1)
;;                   (window-height . 0.3)
;;                   (window-parameters . ((no-other-window . nil)))))
;;   (popper-echo-mode +1)
;;   (add-to-list 'display-buffer-alist
;;                (cons "\\*Async.*" (cons #'display-buffer-no-window nil))))

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

;;;; Messages: попап и posframe

(use-package posframe
  :ensure t
  :defer t)

(defun pro/показать-сообщения ()
  "Показать буфер *Messages* в нижнем сайд-окне (над echo-area)."
  (interactive)
  (let ((buf (get-buffer-create "*Messages*")))
    (display-buffer
     buf
     '((display-buffer-in-side-window)
       (side . bottom)
       (slot . 0)
       (window-height . 0.3)
       (window-parameters . ((no-other-window . nil)))))))

(defun pro/скрыть-сообщения ()
  "Скрыть нижнее окно с *Messages*."
  (interactive)
  (when-let ((win (get-buffer-window "*Messages*" t)))
    (delete-window win)))

(defvar messages-posframe--frame nil
  "Child frame, созданный для показа *Messages* через posframe.")

(defun messages-posframe--auto-hide ()
  "Скрыть posframe, если фокус ушёл из него."
  (when (and messages-posframe--frame
             (frame-live-p messages-posframe--frame)
             (not (eq (selected-frame) messages-posframe--frame)))
    (скрыть-Messages-posframe)))

(defun показать-сообщения-в-posframe (&optional lines)
  "Показать *Messages* в posframe внизу экрана, над echo-area.
LINES — желаемая высота в строках (по умолчанию 15). Posframe
исчезает при клике вне его, но позволяет выделение и прокрутку."
  (interactive "P")
  (require 'posframe)
  (let* ((buf (get-buffer-create "*Messages*"))
         (height (cond
                  ((numberp lines) (max 5 (min (frame-height) lines)))
                  ((consp lines) (max 5 (min (frame-height) (car lines))))
                  (t 15)))
         (width (frame-width))
         (mbw (minibuffer-window (selected-frame)))
         (mbh (if (and mbw (window-live-p mbw))
                  (window-pixel-height mbw)
                0))
         (y-off (- mbh)))
    (setq messages-posframe--frame
          (posframe-show buf
                         :poshandler #'posframe-poshandler-frame-bottom-center
                         :y-pixel-offset y-off
                         :width width
                         :height height
                         :respect-mode-line nil
                         :respect-header-line nil
                         :override-parameters
                         '((no-accept-focus . nil)
                           (minibuffer . nil)
                           (drag-internal-border . t)
                           (internal-border-width . 1)
                           (cursor-type . t))))
    (when (frame-live-p messages-posframe--frame)
      (select-frame-set-input-focus messages-posframe--frame)
      (with-current-buffer buf
        (goto-char (point-max)))
      (add-hook 'post-command-hook #'messages-posframe--auto-hide))))

(defun скрыть-сообщения-posframe ()
  "Скрыть posframe с *Messages*."
  (interactive)
  (when (and (featurep 'posframe)
             (frame-live-p messages-posframe--frame))
    (posframe-hide "*Messages*")
    (setq messages-posframe--frame nil)
    (remove-hook 'post-command-hook #'messages-posframe--auto-hide)))

(provide 'про-окна)
;;; про-окна.el ends here
