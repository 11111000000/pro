;;; графическая-среда.el --- Оконный менеджер ExWM
;;; Commentary:
;; Emacs X Window Manager - полноценный тайловый оконный менеджер
;;; Code:
;;;; Xelb

(use-package xelb
  :ensure t
  :if window-system
  )

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
  "Получить скриншот области."
  (interactive)
  (sit-for 1)
  (async-shell-command
   "scrot '/home/az/Скриншоты/%Y-%m-%d-%H-%M_$wx$h.png' -s -e 'xclip -selection clipboard -target image/png -i $f' &" nil nil))

(defun скриншот ()
  "Получить скриншот."
  (interactive)
  (sit-for 1)
  (async-shell-command "scrot '/home/az/Скриншоты/%Y-%m-%d-%H-%M_$wx$h.png'" nil nil))


(defmacro exwm-input-set-keys (&rest key-bindings)
  "Макрос для установки клавиш, работающих поверх приложений Xorg.
KEY-BINDINGS - список пар (клавиша функция)"
  `(dolist (kb ',key-bindings)
     (cl-destructuring-bind (key cmd) kb
       (exwm-input-set-key (kbd key) cmd))))

(use-package exwm
  :ensure t
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
              (exwm-workspace-rename-buffer exwm-class-name)))
  
  ;; (defun exwm-update-title-hook ()
  ;;   (exwm-workspace-rename-buffer exwm-title))

  ;; (add-hook 'exwm-update-title-hook 'exwm-update-title-hook)


  ;; Глобальные клавиши над всеми приложениями
  
  (dotimes (i 10)
    (exwm-input-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (exwm-workspace-switch-create ,i))))
  (exwm-input-set-keys
   ("s-q" exwm-reset)
   ("s-\\" toggle-input-method)
   ("s-w" exwm-workspace-switch)
   ("s-e" buffer-expose)
   ("s-t" exwm-floating-toggle-floating)
   ("s-r" rename-buffer)
   ("s-d" delete-window)
   ("s-h" windmove-left)
   ("s-j" windmove-down)
   ("s-k" windmove-up)
   ("s-l" windmove-right)
   ("s-H" buf-move-left)
   ("s-J" buf-move-down)
   ("s-K" buf-move-up)
   ("s-L" buf-move-right)
   ("s-x" app-launcher-run-app)
   ("s-M-h" split-window-horizontally)
   ("s-M-j" (lambda () (interactive) (split-window-vertically) (windmove-down)))
   ("s-M-k" split-window-vertically)
   ("s-M-l" (lambda () (interactive) (split-window-horizontally) (windmove-right)))
   ("<XF86Back>" winner-undo)
   ("<XF86Forward>" winner-redo)
   ("<XF86AudioMicMute>" (lambda () (interactive) (async-shell-command "pamixer --default-source 1 -t" nil nil)))
   ("<XF86AudioMute>" (lambda () (interactive) (async-shell-command "pamixer -t" nil nil)))
   ("<XF86AudioRaiseVolume>" (lambda () (interactive) (async-shell-command "pamixer -i 10" nil nil)))
   ("<XF86AudioLowerVolume>" (lambda () (interactive) (async-shell-command "pamixer -d 10" nil nil)))
   ("<XF86MonBrightnessUp>" (lambda () (interactive) (async-shell-command "echo ok" nil nil)))
   ("<XF86MonBrightnessDown>" (lambda () (interactive) (async-shell-command "echo ok" nil nil)))
   ("<XF86TouchpadToggle>" (lambda () (interactive) (async-shell-command "xinput toggle 13" nil nil)))
   ("<print>" скриншот-области)
   ("s-s" скриншот-области)
   ("s-<print>" скриншот)
   ("s-S-s" скриншот)
   ("s-!" (lambda () (interactive) (exwm-workspace-move-window 1)))
   ("s-@" (lambda () (interactive) (exwm-workspace-move-window 2)))
   ("s-#" (lambda () (interactive) (exwm-workspace-move-window 3)))
   ("s-$" (lambda () (interactive) (exwm-workspace-move-window 4)))
   ("s-%" (lambda () (interactive) (exwm-workspace-move-window 5)))
   ("s-^" (lambda () (interactive) (exwm-workspace-move-window 6)))
   ("s-&" (lambda () (interactive) (exwm-workspace-move-window 7)))
   ("s-)" (lambda () (interactive) (exwm-workspace-move-window 0)))
   ("s-<left>" shrink-window-horizontally)
   ("s-<right>" enlarge-window-horizontally)
   ("s-<down>" shrink-window)
   ("s-<up>" enlarge-window)
   ("s-<tab>" consult-buffer)
   ("s-f" ace-window)
   ("s-z" avy-goto-char)
   ("s-_ " winner-undo)
   ("s-M-_" winner-redo)
   ("s-u " winner-undo)
   ("s-S-u" winner-redo)
   ("s-<f3>" battery)
   ("s-`" vterm-toggle) ;; TODO urxvt-toggle
   ("C-`" scratch-pop)
   ("s-a" buffer-expose)
   ("s-SPC" buffer-expose)
   ("s-*" buffer-expose-stars)
   ("s-o" (lambda () (interactive) (показать-окно-в-попапе "chrome app")))
   ("s-O" (lambda () (interactive) (показать-окно-в-попапе "chrome dev"))))
  
  (setq exwm-manage-configurations '(((equal exwm-title "posframe") floating t floating-mode-line nil)
                                     ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
                                      height 175 x 30 y 30 managed t)))
  :init

  ;; Запуск EXWM

  (exwm-enable t)

  ;; Запуск программ в трее

  (require 'exwm-systemtray)
  
  (exwm-systemtray-enable)
  
  (eval-after-load 'exwm-systemtray
    (progn
      (start-process-shell-command "pasystray" nil "dbus-launch pasystray")
      (start-process-shell-command "nm-applet" nil "dbus-launch nm-applet -t")
      (start-process-shell-command "blueman-applet" nil "dbus-launch blueman-applet")
      (start-process-shell-command "udiskie" nil "dbus-launch udiskie -t")
      (start-process-shell-command "dunst" nil "dbus-launch dunst -conf ~/System/dunstrc"))))

;;;; Режимы ввода EMACS в приложениях

;; В EMACS по-умолчанию раскладка переключается сочетанием C-\
;; exim позволяет использовать стандартные режимы ввода EMACS во всех приложениях Xorg

(use-package exim
  :init (установить-из-репы :repo "ch11ng/exim")
  :after (exwm)
  :if window-system
  ;; :load-path "emacs-lisp/exim/exim"
  :hook ((exwm-init . exim-start))
  :config (push ?\C-\\ exwm-input-prefix-keys))

;;;; Редактирование любых полей ввода через EMACS

(use-package exwm-edit
  :if window-system
  :ensure t
  :config
  ;; (defun ag-exwm/on-exwm-edit-compose ()
  ;;   (spacemacs/toggle-visual-line-navigation-on)
  ;;   (funcall 'markdown-mode))
  ;; (add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose)
  )

(use-package exwm-mff
  :if window-system
  :ensure t
  :init
  (exwm-mff-mode -1))

(provide 'графическая-среда)
;;; графическая-среда.el ends here
