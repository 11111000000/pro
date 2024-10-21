;;; про-терминалы.el --- Терминалы -*- lexical-binding: t -*-
;;; Commentary:

;; Конфигурация терминалов

;;; Code:
;;;; Оболочка Emacs Shell
(use-package eshell
  :defer t
  :ensure t
  :defines (eshell-mode-map)
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
  :defer t 
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
  :defer t 
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
  :defer t 
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell))

;;;; Терминал VTerm

(use-package vterm
  :defer t 
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :defines (vterm-mode-map)
  :custom ((vterm-shell  "bash")
          (vterm-kill-buffer-on-exit t)
          (vterm-disable-bold-font t)
          (vterm-term-environment-variable "xterm-256color" ))
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command) ;; TODO
                ("C-\\" . #'toggle-input-method)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank))
  :config

  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
             (yank-undo-function (lambda (_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     (lambda (str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  
  (advice-add 'consult-yank-from-kill-ring :around #'vterm-counsel-yank-pop-action)
  
  (defface terminal-face
    '((((background light)) (:family "Terminus (TTF)" :height 150))
      (((background dark)) (:family "Terminus (TTF)"  :height 150)))
    "Terminal face")

  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  
  (defun set-vterm-font ()
    (set (make-local-variable 'buffer-face-mode-face) 'terminal-face)
    (buffer-face-mode t)
    ;; (set-background-color "black")
    ;; (set-foreground-color "white")
    ;; (set-face-background 'vterm-color-black "black")
    ;; (set-face-foreground 'vterm-color-white "white")
    ;; (set-face-attribute 'vterm-color-black nil :foreground "#073642" :background "#002b36")
    ;; (set-face-attribute 'vterm-color-red nil :foreground "#dc322f" :background "#cb4b16")
    ;; (set-face-attribute 'vterm-color-green nil :foreground "#859900" :background "#586e75")
    ;; (set-face-attribute 'vterm-color-yellow nil :foreground "#b58900" :background "#657b83")
    ;; (set-face-attribute 'vterm-color-blue nil :foreground "#268bd2" :background "#839496")
    ;; (set-face-attribute 'vterm-color-magenta nil :foreground "#d33682" :background "#6c71c4")
    ;; (set-face-attribute 'vterm-color-cyan nil :foreground "#2aa198" :background "#93a1a1")
    ;; (set-face-attribute 'vterm-color-white nil :foreground "#eee8d5" :background "#fdf6e3")
    ;; (face-remap-add-relative
    ;;  'default
    ;;  :foreground "#000000"
    ;;  :background "#000000")
    ;; (face-remap-add-relative
    ;;  'fringe
    ;;  :foreground "#ffffff"
    ;;  :background "#000000")
    )
  :hook
  (vterm-mode . turn-off-chrome)
  (vterm-mode . set-vterm-font))

(use-package eterm-256color
  :defer t 
  :ensure t)

(use-package multi-vterm
  :defer t 
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
          (multi-vterm-dedicated-open))))))

(provide 'про-терминалы)
;;; про-терминалы.el ends here
