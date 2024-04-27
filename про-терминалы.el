;;; про-терминалы.el --- Терминалы
;;; Commentary:

;; Конфигурация терминалов

;;; Code:
;;;; Оболочка Emacs Shell
(use-package eshell
  :ensure t
  :custom
  (eshell-prompt-function 'приглашение-eshell)
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
                                 (define-key eshell-mode-map [down] 'next-line)))))

;; Подсветка синтаксиса в Eshell

(use-package eshell-syntax-highlighting
  :init (установить-из :repo "akreisher/eshell-syntax-highlighting")
  :functions (eshell-syntax-highlighting-global-mode)
  :config
  (eshell-syntax-highlighting-global-mode 1))

;; Предпросмотр а-ля в Plan9

;; (use-package em-smart
;;   :ensure t
;;   :custom
;;   (eshell-where-to-jump 'begin)
;;   (eshell-review-quick-commands nil)
;;   (eshell-smart-space-goes-to-end t))

;; Сокращалка путей

(use-package shrink-path
  :ensure t
  :demand t)

(defun приглашение-eshell ()
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
             (propertize "❯❯❯" 'face `(:foreground "#ff79c6")))))

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
  (eshell-toggle-init-function #'eshell-toggle-init-eshell))

;;;; Терминал VTerm

(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :defines (vterm-mode-map)
  :custom ((vterm-shell  "bash")
          (vterm-kill-buffer-on-exit t)
          (vterm-disable-bold-font t)
          (vterm-term-environment-variable "xterm-256color" )
	      )
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command) ;; TODO
                ("C-\\" . #'toggle-input-method)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank))
  :config
  
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  
  (defface terminal-face 
    '((((background light)) (:background "#220000" :family "Terminus (TTF)" :height 1.1))
      (((background dark)) (:background "#220000" :family "Terminus (TTF)"  :height 1.1)))
    "Terminal face")

  (defun set-vterm-font ()
    (set (make-local-variable 'buffer-face-mode-face) 'terminal-face)
    (buffer-face-mode t)
    (face-remap-add-relative 'default '(:foreground "#ffffff" :background "#000000")))

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
  :config)

(use-package multi-vterm
  :ensure t
  :config)

(require 'projectile)

(defun открыть-терминал-проекта ()
  "Открыть терминал проекта или директории."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (delete-window)
    (let ((окно-терминала (cl-find-if
                          (lambda (window)
                            (with-current-buffer (window-buffer window) (eq major-mode 'vterm-mode)))
                          (window-list))))
      (if окно-терминала
          (select-window окно-терминала)
        (if (projectile-project-p)
            (progn
              (split-window-below)
              (windmove-down)
              (projectile-run-vterm))
          (vterm-toggle))))))

(provide 'про-терминалы)
;;; терминалы.el ends here
