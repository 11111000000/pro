;;; про-окна.el --- Управление окнами в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: window, buffer, golden-ratio, popper, buffer-expose
;; URL: https://github.com/username/emacs.d/blob/main/интерфейс/про-окна.el
;;
;;; Commentary:
;;
;; Этот файл настраивает управление окнами в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Окна — основной способ организации рабочего
;; пространства. Здесь мы добавляем перемещение окон, золотое сечение
;; для фокусировки, обзор всех буферов и popper для предсказуемых попапов.
;; Это делает работу с несколькими буферами удобной и эффективной.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Перемещение окон (buffer-move)
;;  2. Золотое сечение (golden-ratio)
;;  3. Обзор буферов (buffer-expose)
;;  4. Popper для попапов
;;  5. Дополнительные утилиты
;;  6. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-окна) в init.el.
;; Рекомендуется подключать после про-внешний-вид и про-буферы.
;;
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
  :custom
  (popper-window-height 20)
  (popper-display-control nil)
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
          "^\\*term.*\\*$"
          term-mode
          "^\\*vterm.*\\*$"
          "^\\*gptel.*\\*$"
          "^\\*AI.*\\*$"
          vterm-mode
          gptel-aibo-mode
          gptel-mode
          "^\\*nixos-log\\*$"))
  (popper-mode +1)
  ;; GPTel: открывать справа (и по имени, и по major-mode)
  (add-to-list 'display-buffer-alist
               '( (lambda (buf _action)
                    (with-current-buffer buf
                      (or (derived-mode-p 'gptel-mode 'gptel-aibo-mode)
                          (string-match-p "^\\*gptel.*\\*$" (buffer-name)))))
                  (display-buffer-reuse-window display-buffer-in-side-window)
                  (side . top)
                  (slot . 1)
                  (window-width . 0.35)
                  (window-parameters . ((no-other-window . nil)))))
  ;; vterm: открывать снизу (и по имени, и по major-mode)
  (add-to-list 'display-buffer-alist
               '( (lambda (buf _action)
                    (with-current-buffer buf
                      (or (derived-mode-p 'vterm-mode)
                          (string-match-p "^\\*vterm.*\\*$" (buffer-name)))))
                  (display-buffer-reuse-window display-buffer-in-side-window)
                  (side . bottom)
                  (slot . 1)
                  (window-height . 0.3)
                  (window-parameters . ((no-other-window . nil)))))

  ;; Org Capture: открывать в верхнем сайд-окне, чтобы не ломать раскладку окон
  (add-to-list 'display-buffer-alist
               '( (lambda (buf _action)
                    (with-current-buffer buf
                      (or (and (boundp 'org-capture-mode) (derived-mode-p 'org-capture-mode))
                          (string= (buffer-name) "*Org Select*")
                          (string= (buffer-name) "*Capture*"))))
                  (display-buffer-reuse-window display-buffer-in-side-window)
                  (side . top)
                  (slot . 0)
                  (window-height . 0.3)
                  (window-parameters . ((no-other-window . t)
                                        (no-delete-other-windows . t)))))
  (popper-echo-mode +1)
  (add-to-list 'display-buffer-alist
               (cons "\\*Async.*" (cons #'display-buffer-no-window nil))))

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

;; (defvar messages-posframe--frame nil
;;   "Child frame, созданный для показа *Messages* через posframe.")

;; (defun messages-posframe--auto-hide ()
;;   "Скрыть posframe, если фокус ушёл из него."
;;   (when (and messages-posframe--frame
;;              (frame-live-p messages-posframe--frame)
;;              (not (eq (selected-frame) messages-posframe--frame)))
;;     (скрыть-сообщения-posframe)))

;; (defun показать-сообщения-в-posframe (&optional lines)
;;   "Показать *Messages* в posframe внизу экрана, над echo-area.
;; LINES — желаемая высота в строках (по умолчанию 15). Posframe
;; исчезает при клике вне его, но позволяет выделение и прокрутку."
;;   (interactive "P")
;;   (require 'posframe)
;;   (let* ((buf (get-buffer-create "*Messages*"))
;;          (height (cond
;;                   ((numberp lines) (max 5 (min (frame-height) lines)))
;;                   ((consp lines) (max 5 (min (frame-height) (car lines))))
;;                   (t 15)))
;;          (width (frame-width))
;;          (mbw (minibuffer-window (selected-frame)))
;;          (mbh (if (and mbw (window-live-p mbw))
;;                   (window-pixel-height mbw)
;;                 0))
;;          (y-off (- mbh)))
;;     (setq messages-posframe--frame
;;           (posframe-show buf
;;                          :poshandler #'posframe-poshandler-frame-bottom-center
;;                          :y-pixel-offset y-off
;;                          :width width
;;                          :height height
;;                          :respect-mode-line nil
;;                          :respect-header-line nil
;;                          :override-parameters
;;                          '((no-accept-focus . nil)
;;                            (minibuffer . nil)
;;                            (drag-internal-border . t)
;;                            (internal-border-width . 1)
;;                            (cursor-type . t))))
;;     (when (frame-live-p messages-posframe--frame)
;;       (select-frame-set-input-focus messages-posframe--frame)
;;       (with-current-buffer buf
;;         (goto-char (point-max)))
;;       (add-hook 'post-command-hook #'messages-posframe--auto-hide))))

;; (defun скрыть-сообщения-posframe ()
;;   "Скрыть posframe с *Messages*."
;;   (interactive)
;;   (when (and (featurep 'posframe)
;;              (frame-live-p messages-posframe--frame))
;;     (when-let ((buf (get-buffer "*Messages*")))
;;       (posframe-hide buf))
;;     (setq messages-posframe--frame nil)
;;     (remove-hook 'post-command-hook #'messages-posframe--auto-hide)))

;; Ensure org-projectile capture uses a top side window without breaking layout
;; (defun my/org-projectile-capture-top--around (orig-fun &rest args)
;;   (let ((display-buffer-alist
;;          (cons
;;           `(,(lambda (buf _)
;;                (with-current-buffer buf
;;                  (or (eq major-mode 'org-capture-mode)
;;                      (eq major-mode 'org-capture-select-template-mode))))
;;             (display-buffer-in-side-window)
;;             (side . top)
;;             (window-height . 0.33)
;;             (slot . 0)
;;             (window-parameters . ((no-other-window . t)
;;                                   (no-delete-other-windows . t))))
;;           display-buffer-alist)))
;;     (apply orig-fun args)))

;; (with-eval-after-load 'org-projectile
;;   (advice-add 'org-projectile-capture-for-current-project :around #'my/org-projectile-capture-top--around))

;; Org/Org-Projectile capture: всегда сверху и без ломания раскладки
;; (with-eval-after-load 'org
;;   (add-to-list 'display-buffer-alist
;;                '((lambda (buf _)
;;                    (let ((name (if (bufferp buf) (buffer-name buf) buf)))
;;                      (or (string= name "*Org Select*")
;;                          (string= name "*Capture*")
;;                          (string-prefix-p "*Capture*" name)
;;                          (string= name "*Org Capture*"))))
;;                  (display-buffer-in-side-window)
;;                  (side . top)
;;                  (slot . 0)
;;                  (window-height . 0.33)
;;                  (inhibit-same-window . t)
;;                  (window-parameters . ((no-other-window . t)
;;                                        (no-delete-other-windows . t))))))

;; (with-eval-after-load 'org
;;   (add-to-list 'display-buffer-alist
;;                '((lambda (buf _)
;;                    (let ((name (buffer-name buf)))
;;                      (or (string= name "*Org Select*")
;;                          (string= name "*Capture*")
;;                          (string-prefix-p "*Capture*" name)
;;                          (string= name "*Org Capture*"))))
;;                  (display-buffer-in-side-window)
;;                  (side . top)
;;                  (slot . 0)
;;                  (window-height . 0.33)
;;                  (inhibit-same-window . t)
;;                  (window-parameters . ((no-other-window . t)
;;                                        (no-delete-other-windows . t))))))

;; (with-eval-after-load 'org-projectile
;;   (advice-add 'org-projectile-capture-for-current-project :around
;;               (lambda (orig-fun &rest args)
;;                 (let ((display-buffer-overriding-action
;;                        '((display-buffer-reuse-window display-buffer-in-side-window)
;;                          (side . top)
;;                          (slot . 0)
;;                          (window-height . 0.33)
;;                          (inhibit-same-window . t)
;;                          (window-parameters . ((no-other-window . t)
;;                                                (no-delete-other-windows . t))))))
;;                   (apply orig-fun args)))))

(provide 'про-окна)
;;; про-окна.el ends here
