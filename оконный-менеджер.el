;;; Xorg

;; Emacs X Window Manager - полноценный тайловый оконный менеджер

;;;; Xelb

(use-package xelb 
  :ensure t 
  :if window-system 
  ;; :load-path "emacs-lisp/xelb/"
  )

;;;; ExWM

(defvar az/default-simulation-keys 
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

;; (exwm-input-set-simulation-keys az/default-simulation-keys)

(use-package exwm 
  :ensure t
  :if window-system 
  :custom ((exwm-workspace-number 5) 
           (exwm-workspace-show-all-buffers t) 
           (exwm-layout-show-all-buffers t) 
           (exwm-manage-force-tiling nil) 
           (exwm-systemtray-height 16)
           (exwm-input-simulation-keys az/default-simulation-keys))
  
  :config
  (add-hook 'exwm-update-class-hook 
            (lambda 
              () 
              (exwm-workspace-rename-buffer exwm-class-name)))
  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; Глобальные клавиши над всеми приложениями
  (dotimes (i 10) 
    (exwm-input-set-key (kbd (format "s-%d" i)) 
                        `(lambda 
                           () 
                           (interactive) 
                           (exwm-workspace-switch-create ,i)))) 
  (exwm-input-set-key (kbd "s-&") 
                      (lambda 
                        (command) 
                        (interactive (list (read-shell-command "$ "))) 
                        (start-process-shell-command command nil command))) 
  (exwm-input-set-key (kbd "s-q") #'exwm-reset) 
  (exwm-input-set-key (kbd "s-\\") #'toggle-input-method) 
  (exwm-input-set-key (kbd "s-w") #'exwm-workspace-switch) 
  (exwm-input-set-key (kbd "s-e") 'buffer-expose) 
  (exwm-input-set-key (kbd "s-t") 'exwm-floating-toggle-floating) 
  (exwm-input-set-key (kbd "s-r") 'rename-buffer) 
  (exwm-input-set-key (kbd "s-d") 'delete-window) 
  (exwm-input-set-key (kbd "s-h") 'windmove-left) 
  (exwm-input-set-key (kbd "s-j") 'windmove-down) 
  (exwm-input-set-key (kbd "s-k") 'windmove-up) 
  (exwm-input-set-key (kbd "s-l") 'windmove-right) 
  (exwm-input-set-key (kbd "s-H") 'buf-move-left) 
  (exwm-input-set-key (kbd "s-J") 'buf-move-down) 
  (exwm-input-set-key (kbd "s-K") 'buf-move-up) 
  (exwm-input-set-key (kbd "s-L") 'buf-move-right)
  (exwm-input-set-key (kbd "s-x") 'app-launcher-run-app)
  (exwm-input-set-key (kbd "s-M-h") 'split-window-horizontally) 
  (exwm-input-set-key (kbd "s-M-j") 
                      '(lambda 
                         () 
                         (interactive) 
                         (split-window-vertically) 
                         (windmove-down))) 
  (exwm-input-set-key (kbd "s-M-k") 'split-window-vertically) 
  (exwm-input-set-key (kbd "s-M-l") 
                      '(lambda 
                         () 
                         (interactive) 
                         (split-window-horizontally) 
                         (windmove-right))) 
  (exwm-input-set-key (kbd "<XF86Back>") 'winner-undo) 
  (exwm-input-set-key (kbd "<XF86Forward>") 'winner-redo) 
  (exwm-input-set-key (kbd "<XF86AudioMicMute>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "pamixer --default-source 1 -t" nil nil))) 
  (exwm-input-set-key (kbd "<XF86AudioMute>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "pamixer -t" nil nil))) 
  (exwm-input-set-key (kbd "<XF86AudioRaiseVolume>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "pamixer -i 10" nil nil))) 
  (exwm-input-set-key (kbd "<XF86AudioLowerVolume>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "pamixer -d 10" nil nil))) 
  (exwm-input-set-key (kbd "<XF86MonBrightnessUp>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "echo ok" nil nil))) 
  (exwm-input-set-key (kbd "<XF86MonBrightnessDown>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "echo ok" nil nil))) 
  (exwm-input-set-key (kbd "<XF86TouchpadToggle>") 
                      (lambda 
                        () 
                        (interactive) 
                        (async-shell-command "xinput toggle 13" nil nil))) 

  (defun dobro/take-screenshot-region ()
    "Получить скриншот области."
    (interactive)
    (sit-for 1) 
    (async-shell-command
     "scrot '/home/az/Скриншоты/%Y-%m-%d-%H-%M_$wx$h.png' -s -e 'xclip -selection clipboard -target image/png -i $f' &"
     nil nil))
  
  (defun dobro/take-screenshot ()
    "Получить скриншот."
    (interactive)
    (sit-for 1)
    (async-shell-command "scrot '/home/az/Скриншоты/%Y-%m-%d-%H-%M_$wx$h.png'" nil nil))
  
  (exwm-input-set-key (kbd "<print>") 'dobro/take-screenshot-region)
  (exwm-input-set-key (kbd "s-s") 'dobro/take-screenshot-region)
  (exwm-input-set-key (kbd "s-<print>") 'dobro/take-screenshot)
  (exwm-input-set-key (kbd "s-S-s") 'dobro/take-screenshot)
  
  (exwm-input-set-key (kbd "s-!") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 1))) 
  (exwm-input-set-key (kbd "s-@") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 2))) 
  (exwm-input-set-key (kbd "s-#") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 3))) 
  (exwm-input-set-key (kbd "s-$") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 4))) 
  (exwm-input-set-key (kbd "s-%") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 5))) 
  (exwm-input-set-key (kbd "s-^") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 6))) 
  (exwm-input-set-key (kbd "s-&") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 7))) 
  (exwm-input-set-key (kbd "s-)") 
                      (lambda 
                        () 
                        (interactive) 
                        (exwm-workspace-move-window 0))) 
  (exwm-input-set-key (kbd "s-<left>") 'shrink-window-horizontally) 
  (exwm-input-set-key (kbd "s-<right>") 'enlarge-window-horizontally) 
  (exwm-input-set-key (kbd "s-<down>") 'shrink-window) 
  (exwm-input-set-key (kbd "s-<up>") 'enlarge-window)

                                        ;(exwm-input-set-key (kbd "s-b") 'helm-exwm)
  (exwm-input-set-key (kbd "s-<tab>") 'consult-buffer)
  (exwm-input-set-key (kbd "s-f") 'ace-window)
  (exwm-input-set-key (kbd "s-z") 'avy-goto-char)
  (exwm-input-set-key (kbd "s-_ ") 'winner-undo) 
  (exwm-input-set-key (kbd "s-M-_") 'winner-redo) 
  (exwm-input-set-key (kbd "s-<f3>") 'battery) 
  (exwm-input-set-key (kbd "s-`") 'eshell-toggle) ;; TODO urxvt-toggle
  (exwm-input-set-key (kbd "C-`") 'scratch-pop) 
  (exwm-input-set-key (kbd "s-a") 'buffer-expose) 
  (exwm-input-set-key (kbd "s-SPC") 'buffer-expose) 
  (exwm-input-set-key (kbd "s-*") 'buffer-expose-stars)
  ;;(exwm-input-set-key (kbd "s-v") 'rotate-layout)

  ;;(exwm-input-set-key (kbd "s-o") '(lambda () (interactive)
  ;;                                    (om/popwin-toggle-name "ff")))

  ;; (exwm-input-set-key (kbd "s-i") '(lambda () (interactive)
  ;;                                    (om/popwin-toggle-name "ff dev")))
  (exwm-input-set-key (kbd "s-o") 
                      '(lambda 
                         () 
                         (interactive) 
                         (om/popwin-toggle-name "chrome app"))) 
  (exwm-input-set-key (kbd "s-O") 
                      '(lambda 
                         () 
                         (interactive) 
                         (om/popwin-toggle-name "chrome dev")))
  ;; (exwm-input-set-key (kbd "s-I") '(lambda () (interactive)
  ;;                                    (om/popwin-toggle-name "chrome dev2")))
  ;; (exwm-input-set-key (kbd "s-c 1") '(lambda () (interactive)
  ;;                                      (om/popwin-toggle-name "1")))
  ;; (exwm-input-set-key (kbd "s-c 2") '(lambda () (interactive)
  ;;                                      (om/popwin-toggle-name "2")))
  ;; (exwm-input-set-key (kbd "s-c 3") '(lambda () (interactive)
  ;;                                      (om/popwin-toggle-name "3")))
  ;; (exwm-input-set-key (kbd "s-c 4") '(lambda () (interactive)
  ;;                                      (om/popwin-toggle-name "4")))
  ;; (exwm-input-set-key (kbd "s-c 5") '(lambda () (interactive)
  ;;                                      (om/popwin-toggle-name "5")))
  ;; (setq exwm-manage-configurations
  ;;       '())
  (setq exwm-manage-configurations '(((equal exwm-title "posframe") floating t floating-mode-line nil) 
                                     ((equal exwm-class-name "chromebug") floating t floating-mode-line nil width 280
                                      height 175 x 30 y 30 managed t))) 
  :init

  ;; Запуск EXWM
  
  (exwm-enable t)

  ;; Запуск программ в трее
  
  (start-process-shell-command "pasystray" nil "dbus-launch pasystray") 
  (start-process-shell-command "nm-applet" nil "dbus-launch nm-applet -t") 
  (start-process-shell-command "blueman-applet" nil "dbus-launch blueman-applet") 
  (start-process-shell-command "udiskie" nil "dbus-launch udiskie -t") 
  (start-process-shell-command "dunst" nil "dbus-launch dunst -conf ~/System/dunstrc")

  )


;;;; Режимы ввода EMACS в приложениях

;; В EMACS по-умолчанию раскладка переключается сочетанием C-\
;; exim позволяет использовать стандартные режимы ввода EMACS во всех приложениях Xorg

(use-package exim 
  :after (exwm) 
  :if window-system 
  ;; :load-path "emacs-lisp/exim/exim" 
  :hook ((exwm-init . exim-start)) 
  :init (push ?\C-\\ exwm-input-prefix-keys))

;; Кстати, этот модуль предварительно надо скачать (по какой-то причине его нет в поставке ExWM)

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
  (exwm-mff-mode -1)
  )


(provide 'оконный-менеджер)
