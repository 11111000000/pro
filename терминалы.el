;;; терминалы.el --- Терминалы
;;; Commentary:

;; Конфигурация терминалов

;;; Code:
;;;; Мульти-терминалы

(use-package multi-term
  :ensure t
  :defer t
  :bind (
         ("C-c tt" . multi-term)
         ;; ("<s-return>" . multi-term)
         ("C-c tn" . multi-term-next)
         ("C-c tp" . multi-term-prev)
         ("M-±" . multi-term-dedicated-toggle)
         ("C-c to" . multi-term-dedicated-toggle)
         :map term-mode-map
         ("C-c C-j" . переключить-режим-ввода-терминала)
         ("C-c C-k" . переключить-режим-ввода-терминала)
         :map term-raw-map
         ("C-c C-j" . переключить-режим-ввода-терминала)
         ("C-c C-k" . переключить-режим-ввода-терминала))
  :custom ((term-buffer-maximum-size 0)
           (show-trailing-whitespace nil))
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))
              (add-to-list 'term-bind-key-alist '("C-v" . scroll-up-command))
              (add-to-list 'term-bind-key-alist '("M-v" . scroll-down-command))
              ))
  (defun term-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'term-mode)
        (let ((inhibit-read-only t)
             )
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (term-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'consult-yank-from-kill-ring :around #'term-counsel-yank-pop-action)
  )

;; Функция для переключение режима перемещения по терминалу

(defun переключить-режим-ввода-терминала ()
  "Toggle term between line mode and char mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;;;; Выпадающий терминал Eshell

(use-package shrink-path
  :ensure t
  :demand t)

;;;;; Настройка приглашения Eshell

(defun custom-eshell-prompt ()
  "Настройка приглашения оболочки EShell."
  (let* (
         (git-branch-unparsed
          (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
         (git-branch
          (if (string= git-branch-unparsed "")
              ""
            (substring git-branch-unparsed 0 -1)))
         )
     (format "%s %s%s %s\n%s "
             (all-the-icons-octicon "repo")
             (propertize (car (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
             (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
             (unless (string= git-branch "")
               (propertize (concat "[" git-branch "]") 'face `(:inherit font-lock-string-face)))
             (propertize "❯❯❯" 'face `(:foreground "#ff79c6"))
             )))

(use-package eshell
  :ensure t
  :custom
  (eshell-prompt-function 'custom-eshell-prompt)
  (eshell-highlight-prompt nil)
  (eshell-hist-ignoredups t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  (eshell-prompt-regexp "❯❯❯ ")
  (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))

  :init
  (add-hook 'eshell-mode-hook (lambda ()
                                (progn
                                  (define-key eshell-mode-map "\C-a" 'eshell-bol)
                                  (define-key eshell-mode-map [up] 'previous-line)
                                  (define-key eshell-mode-map [down] 'next-line)
                                  ))))

;; (use-package eshell-did-you-mean
;;   :init
;;   (eshell-did-you-mean-setup)
;;   :ensure t)

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  ;; :quelpa
  ;; (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  ("s-~" . eshell-toggle))

(use-package vterm
  :ensure t
  :custom (
          (vterm-shell  "bash")
          (vterm-kill-buffer-on-exit t)
          (vterm-disable-bold-font t))
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command) ;; TODO
                ("C-\\" . #'toggle-input-method)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank))
  :config

  (custom-set-faces  
   `(vterm-color-default ((t (:foreground "white" :background "black" :inherit default))))  
   `(vterm-color-black   ((t (:foreground "black" :background "black" ))))  
   `(vterm-color-blue    ((t (:foreground "blue" :background "black"))))  
   `(vterm-color-cyan    ((t (:foreground "cyan" :background "black"))))  
   `(vterm-color-green   ((t (:foreground "green" :background "black"))))  
   `(vterm-color-magenta ((t (:foreground "magenta" :background "black"))))  
   `(vterm-color-red     ((t (:foreground "red" :background "black"))))  
   `(vterm-color-white   ((t (:foreground "white" :background "black"))))  
   `(vterm-color-yellow  ((t (:foreground "yellow" :background "black"))))  
   )
  
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  
  (defun set-vterm-font ()
    (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
    (buffer-face-mode t))
  
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
             (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))

  (advice-add 'consult-yank-from-kill-ring :around #'vterm-counsel-yank-pop-action)

  
  
  :hook
  (vterm-mode . turn-off-chrome)
  (vterm-mode . set-vterm-font))

(use-package vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-fullscreen-p nil)
  (vterm-toggle-scope 'project)
  (vterm-toggle-hide-method 'delete-window)
  :config
  (add-to-list 'display-buffer-alist
     '("\*vterm\*"
       (display-buffer-in-side-window)
       (window-height . 0.38)
       (side . bottom)
       (slot . 0))))


(use-package eshell-vterm
  ;;:hook ((eshell-mode . eshell-vterm-mode))
  :ensure t
  :init)

(provide 'терминалы)
;;; терминалы.el ends here
