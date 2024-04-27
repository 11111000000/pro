;;; про-графическую-среду.el --- Оконный менеджер ExWM  -*- lexical-binding: t -*-
;;; Commentary:

;; EXWM - Emacs X Window Manager, тайловый оконный менеджер

;;; Code:

;;;; Xelb

(use-package xelb
  :ensure t
  :if window-system)

;;;; ExWM

(defvar сочетания-для-эмуляции
    '(([?\C-b] . left)
      ([?\M-b] . C-left)
      ([?\C-f] . right)
      ([?\M-f] . C-right)
      ([?\C-p] . up)
      ([?\C-n] . down)
      ([?\C-a] . home)
      ([?\C-e] . end)
      ([?\M-v] . prior)
      ([?\C-v] . next)
      ([?\C-d] . ?\C-x)
      ([?\M-d] . (C-S-right delete))
      ;; cut/paste.
      ([?\M-y] . ?\C-c)
      ([?\M-w] . ?\C-c)
      ([?\C-y] . ?\C-v)
      ;; search
      ([?\C-s] . ?\C-f)))

(defun скриншот-области ()
    "Получить скриншот области и скопировать полученое изоббражение в буфер обмена."
    (interactive)
    (async-shell-command
     "scrot -s '/home/az/Скриншоты/%Y-%m-%d_%H.%M.%S.png' -e 'copyq write image/png - < $f && copyq select 0'" nil nil))

(defun скриншот ()
    "Получить скриншот."
    (interactive)
    (sit-for 1)
    (async-shell-command "scrot '/home/az/Скриншоты/%Y-%m-%d-%H-%M_$wx$h.png' -e 'copyq write image/png - < $f && copyq select 0'" nil nil))

(defmacro exwm-input-set-keys (&rest key-bindings)
    "Макрос для установки клавиш, работающих поверх приложений Xorg.
KEY-BINDINGS - список пар (клавиша функция)"
    `(dolist (kb ',key-bindings)
         (cl-destructuring-bind (key cmd) kb
             (exwm-input-set-key (kbd key) cmd))))

(use-package exwm
  :ensure t
  :functions (exwm-input-set-key exwm-workspace-rename-buffer exwm-enable exwm-systemtray-enable)
  :defines (exwm-class-name exwm-manage-configurations  exwm-title)
  :if window-system
  :custom (
          (exwm-workspace-number 5)
          (exwm-workspace-show-all-buffers t)
          (exwm-layout-show-all-buffers t)
          (exwm-manage-force-tiling nil)
          (exwm-systemtray-height 16)
          (exwm-input-simulation-keys сочетания-для-эмуляции))

  :config

  (add-hook 'exwm-update-class-hook
           (lambda ()
             (exwm-workspace-rename-buffer (concat exwm-class-name exwm-title))))

  (defun exwm-update-title-hook ()
    (exwm-workspace-rename-buffer (concat exwm-class-name ":" exwm-title)))

  (add-hook 'exwm-update-title-hook 'exwm-update-title-hook)

  ;; Глобальные клавиши над всеми приложениями

  (dotimes (i 10)
    (if (> i 0) (exwm-input-set-key (kbd (format "s-M-%d" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i))))
    (exwm-input-set-key (kbd (format "C-s-%d" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "s-<f%d>" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i)))
    )
  (exwm-input-set-key (kbd "s-<f10>") `(lambda () (interactive) (exwm-workspace-switch-create 0)))
  
  (exwm-input-set-keys
   ("s-M-!" (lambda () (interactive) (exwm-workspace-move-window 1)))
   ("s-M-@" (lambda () (interactive) (exwm-workspace-move-window 2)))
   ("s-M-#" (lambda () (interactive) (exwm-workspace-move-window 3)))
   ("s-M-$" (lambda () (interactive) (exwm-workspace-move-window 4)))
   ("s-M-%" (lambda () (interactive) (exwm-workspace-move-window 5)))
   ("s-M-^" (lambda () (interactive) (exwm-workspace-move-window 6)))
   ("s-M-&" (lambda () (interactive) (exwm-workspace-move-window 7)))
   ("s-M-)" (lambda () (interactive) (exwm-workspace-move-window 0))))

  (setq exwm-manage-configurations '(((equal exwm-title "posframe") floating t floating-mode-line nil)
                                    ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
                                     height 175 x 30 y 30 managed t)))
  
  ;;:hook ((after-init . exwm-enable))
  
  :init

  ;; Запуск EXWM

  (exwm-enable t)
  ;;(exwm-init)

  ;; Запуск различных программ в трее
  ;; TODO: Вынести в отдельный файл?

  (require 'exwm-systemtray)

  (exwm-systemtray-enable)
  
  ;; TODO: перенести в подходящее место (м.б. таблицу?)
  (add-hook 'exwm-init-hook (lambda ()
                             (progn
                               (start-process-shell-command "nm-applet" nil "sleep 0.5; dbus-launch nm-applet -t")
                               (start-process-shell-command "blueman-applet" nil "sleep 0.5; dbus-launch blueman-applet")
                               (start-process-shell-command "udiskie" nil "sleep 0.5; dbus-launch udiskie -t")
                               (start-process-shell-command "dunst" nil "sleep 0.5; dbus-launch dunst -conf ~/System/dunstrc")
                               (start-process-shell-command "pasystray" nil "sleep 0.5; dbus-launch pasystray")
                               (start-process-shell-command "copyq" nil "sleep 0.5; copyq"))))
  )

;;;; Режимы ввода EMACS в приложениях

;; В EMACS по-умолчанию раскладка переключается сочетанием C-\
;; exim позволяет использовать стандартные режимы ввода EMACS во всех приложениях Xorg

(require 'установить-из)

(use-package exim
  :init (установить-из :repo "ch11ng/exim")
  :after (exwm)
  :if window-system
  ;; :load-path "emacs-lisp/exim/exim"
  :hook ((exwm-init . exim-start))
  :config (push ?\C-\\ exwm-input-prefix-keys))

;;;; Редактирование любых полей ввода через EMACS

(use-package exwm-edit
  :if window-system
  :ensure t
  :config)

(use-package exwm-mff
  :if window-system
  :functions (exwm-mff-mode)
  :ensure t
  :init
  (exwm-mff-mode t))

;; (use-package exwm-firefox
;;   :if window-system
;;   :ensure t)

(use-package exwm-background
  :init
  (установить-из :repo "pestctrl/exwm-background")
  :config
  ;;(start-process-shell-command "xcompmgr" nil "xcompmgr -c")
  )

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
