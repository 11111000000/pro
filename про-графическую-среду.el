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

(setq-default exwm-debug nil)

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
    (exwm-input-set-key (kbd (format "S-s-<f%d>" 1)) `(lambda () (interactive) (message ">%d" ,i)))
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

;; (require 'exwm)
;; (require 'exwm-manage)                  ;нужен exwm-manage--close-window

;; (defun my/exwm-close-all-windows ()
;;   "Попытаться мягко закрыть все внешние X-приложения, управляемые EXWM."
;;   (interactive)
;;   (dolist (buf (buffer-list))
;;     (with-current-buffer buf
;;       (when (eq major-mode 'exwm-mode)
;;         ;; WM_DELETE_WINDOW → приложение само завершается
;;         (exwm-manage--close-window exwm--id)))))

;; (defun my/exwm-running-x-buffers-p ()
;;   "Есть ли ещё EXWM-буферы (т.е. живые X-клиенты)?"
;;   (seq-some (lambda (buf)
;;               (with-current-buffer buf
;;                 (eq major-mode 'exwm-mode)))
;;             (buffer-list)))

;; (defun my/exwm-shutdown (&optional force)
;;   "Мягкое завершение всех X-приложений, затем Emacs, затем poweroff.
;; Если FORCE не nil, то не задавать вопросов."
;;   (interactive)
;;   ;; 1. Закрываем все внешние окна
;;   (my/exwm-close-all-windows)

;;   ;; 2. Немного ждём; заодно повторяем попытку закрытия «упрямых» клиентов
;;   (dotimes (_ 10)                       ;≈ 10 секунд максимум
;;     (when (my/exwm-running-x-buffers-p)
;;       (sleep-for 1)
;;       (my/exwm-close-all-windows)))

;;   ;; 3. Сохраняем файлы / выходим из Emacs
;;   (when (or force
;;             (yes-or-no-p "Выключить компьютер? "))
;;     ;; Хук, который сработает уже после выхода Emacs
;;     (add-hook 'kill-emacs-hook
;;               (lambda ()
;;                 ;; можно заменить на "shutdown -h now" или "loginctl poweroff"
;;                 (start-process "system-shutdown" nil "systemctl" "poweroff")))
;;     (save-buffers-kill-emacs)))

;; ;; Клавиша быстрого вызова  (Super + Shift + Q, к примеру)
;; (global-set-key (kbd "s-M-q") #'my/exwm-shutdown)

(provide 'про-графическую-среду)
;;; про-графическую-среду.el ends here
