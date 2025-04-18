;;; про-графическую-среду.el --- Оконный менеджер ExWM  -*- lexical-binding: t -*-
;;; Commentary:

;; EXWM - Emacs X Window Manager, тайловый оконный менеджер

;;; Code:

(require 'установить-из)

;;;; Xelb

(use-package xelb
  :ensure t
  :if window-system)

;;;; ExWM

(setq-default exwm-debug t)

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
  ;;:init (установить-из :repo "emacs-exwm/exwm")
  :functions (exwm-input-set-key
         exwm-workspace-rename-buffer
         exwm-enable
         exwm-init
         exwm-systemtray-mode)
  :defines (exwm-class-name
           exwm-manage-configurations
           exwm-title
           exwm-input-prefix-keys)
  :if window-system
  ;;:hook ((exwm-init . exim-start))
  :custom ((exwm-workspace-number 3)
          (exwm-workspace-show-all-buffers t)
          (exwm-layout-show-all-buffers t)
          (exwm-manage-force-tiling nil)
          (exwm-systemtray-height 16)
          (exwm-input-simulation-keys сочетания-для-эмуляции))

  :config
  ;; Смена имени окна
  (add-hook 'exwm-update-class-hook
           (lambda ()
             (exwm-workspace-rename-buffer (concat exwm-class-name exwm-title))))
  ;; Смена заголовка окна
  (defun exwm-update-title-hook ()
    (exwm-workspace-rename-buffer (concat exwm-class-name ":" exwm-title)))

  (add-hook 'exwm-update-title-hook 'exwm-update-title-hook)
  
  ;; Глобальные клавиши над всеми приложениями

  (dotimes (i 9)
    ;;(if (> i 0)
    ;;(exwm-input-set-key (kbd (format "s-M-%d" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i))))
    ;; (exwm-input-set-key (kbd (format "C-s-%d" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "s-<f%d>" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i)))
    (exwm-input-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i)))
    )

  (exwm-input-set-key (kbd "s-<f10>") `(lambda () (interactive) (exwm-workspace-switch-create 0)))

  ;; (exwm-input-set-keys
  ;;  ("s-M-!" (lambda () (interactive) (exwm-workspace-move-window 1)))
  ;;  ("s-M-@" (lambda () (interactive) (exwm-workspace-move-window 2)))
  ;;  ("s-M-#" (lambda () (interactive) (exwm-workspace-move-window 3)))
  ;;  ("s-M-$" (lambda () (interactive) (exwm-workspace-move-window 4)))
  ;;  ("s-M-%" (lambda () (interactive) (exwm-workspace-move-window 5)))
  ;;  ("s-M-^" (lambda () (interactive) (exwm-workspace-move-window 6)))
  ;;  ("s-M-&" (lambda () (interactive) (exwm-workspace-move-window 7)))
  ;;  ("s-M-)" (lambda () (interactive) (exwm-workspace-move-window 0))))

  (setq exwm-manage-configurations '(((equal exwm-title "posframe") floating t floating-mode-line nil)
                                    ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
                                     height 175 x 30 y 30 managed t)))

  :init
  ;; Запуск EXWM
  (exwm-enable)
  (require 'exwm-systemtray)
  (exwm-systemtray-mode)
  (start-process "gnome-keyring-daemon" "*gnome-keyring-daemon*" "gnome-keyring-daemon" "--start"  "--components=pkcs11,ssh,gpg"))

;;;; Режимы ввода EMACS в приложениях

;; В EMACS по-умолчанию раскладка переключается сочетанием C-\
;; exim позволяет использовать стандартные режимы ввода EMACS во всех приложениях Xorg

(use-package exim
  :init (установить-из :repo "ch11ng/exim")
  :after exwm
  :if window-system
  :hook ((exwm-init . exim-start))
  :config
  (push (kbd "C-\\") exwm-input-prefix-keys)
  (push (kbd "s-a") exwm-input-prefix-keys)
  (push (kbd "s-SPC") exwm-input-prefix-keys))

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here

;;;; Редактирование любых полей ввода через EMACS

(use-package exwm-edit
  :after exwm
  :if window-system
  :ensure t
  :config)

;;;; Курсор мыши следует за окном в фокусе

(use-package exwm-mff
  :after exwm
  :if window-system
  :functions (exwm-mff-mode)
  :ensure t
  :init
  (exwm-mff-mode t))

;; (use-package exwm-firefox
;;   :if window-system
;;   :ensure t)

;; (use-package exwm-background
;;   :init
;;   (установить-из :repo "pestctrl/exwm-background")
;;   :config
;;   ;;(start-process-shell-command "xcompmgr" nil "xcompmgr -c")
;;   )

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
