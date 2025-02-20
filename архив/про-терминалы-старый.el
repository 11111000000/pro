;;; про-терминалы.el --- Терминалы -*- lexical-binding: t -*-
;;; Commentary:

;; Конфигурация терминалов

;;; Code:

(require 'установить-из)

;;;; Оболочка Emacs Shell
;; (use-package eshell
;;   :ensure t
;;   :defines (eshell-mode-map)
;;   :custom
;;   (eshell-prompt-function 'приглашение-eshell)
;;   (eshell-highlight-prompt nil)
;;   (eshell-hist-ignoredups t)
;;   (eshell-cmpl-cycle-completions nil)
;;   (eshell-cmpl-ignore-case t)
;;   (eshell-ask-to-save-history (quote always))
;;   (eshell-prompt-regexp "❯❯❯ ")
;;   (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm"))

;;   :init
;;   (add-hook 'eshell-mode-hook (lambda ()
;;                                (progn
;;                                  (define-key eshell-mode-map "\C-a" 'eshell-bol)
;;                                  (define-key eshell-mode-map [up] 'previous-line)
;;                                  (define-key eshell-mode-map [down] 'next-line)))))

;; ;; Подсветка синтаксиса в Eshell

;; (use-package eshell-syntax-highlighting
;;   :init (установить-из :repo "akreisher/eshell-syntax-highlighting")
;;   :functions (eshell-syntax-highlighting-global-mode)
;;   :config
;;   (eshell-syntax-highlighting-global-mode 1))

;; Предпросмотр а-ля в Plan9

;; (use-package em-smart
;;   :ensure t
;;   :custom
;;   (eshell-where-to-jump 'begin)
;;   (eshell-review-quick-commands nil)
;;   (eshell-smart-space-goes-to-end t))

;; Сокращалка путей

;; (use-package shrink-path
;;   :ensure t
;;   :demand t)

;; (defun приглашение-eshell ()
;;   "Настройка приглашения оболочки EShell."
;;   (let* ((git-branch-unparsed
;;          (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
;;         (git-branch
;;          (if (string= git-branch-unparsed "")
;;              ""
;;            (substring git-branch-unparsed 0 -1))))
;;     (format "%s %s%s %s\n%s "
;;           (all-the-icons-octicon "repo")
;;           (propertize (car (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
;;           (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
;;           (unless (string= git-branch "")
;;             (propertize (concat "[" git-branch "]") 'face `(:inherit font-lock-string-face)))
;;           (propertize "❯❯❯" 'face `(:foreground "#33aa33")))))

;; ;; (use-package eshell-did-you-mean
;; ;;   :init
;; ;;   (eshell-did-you-mean-setup)
;; ;;   :ensure t)

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
  :functions (
         vterm-send-next-key
         vterm-yank
         vterm-undo
         vterm-send-string
         vterm-counsel-yank-pop-action)
  :defines (vterm-mode-map)
  :custom ((vterm-shell  "bash")
          (vterm-kill-buffer-on-exit t)
          (vterm-disable-bold-font t)
          (vterm-term-environment-variable "xterm-256color"))
  ;;  :hook
  ;; (vterm-mode . turn-off-chrome)
  ;; (vterm-mode . set-vterm-font)
  ;; (vterm-mode . my-vterm-set-colors)
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command)
                ("C-\\" . #'toggle-input-method)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank))
  :config

  ;; (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  ;;   (if (equal major-mode 'vterm-mode)
  ;;       (let ((inhibit-read-only t)
  ;;            (yank-undo-function (lambda (_start _end) (vterm-undo))))
  ;;         (cl-letf (((symbol-function 'insert-for-yank)
  ;;                    (lambda (str) (vterm-send-string str t))))
  ;;           (apply orig-fun args)))
  ;;     (apply orig-fun args)))
  
  ;; (advice-add 'consult-yank-from-kill-ring :around #'vterm-counsel-yank-pop-action)

  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))

  (defun my-vterm-set-colors ()
    "Set vterm colors to black background and white foreground."
    (set-face-foreground 'vterm-color-black "gray")
    (set-face-background 'vterm-color-black "black")
    (set-face-foreground 'vterm-color-red "red")
    (set-face-background 'vterm-color-red "black")
    (set-face-foreground 'vterm-color-green "green")
    (set-face-background 'vterm-color-green "black")
    (set-face-foreground 'vterm-color-yellow "yellow")
    (set-face-background 'vterm-color-yellow "black")
    (set-face-foreground 'vterm-color-blue "blue")
    (set-face-background 'vterm-color-blue "black")
    (set-face-foreground 'vterm-color-magenta "magenta")
    (set-face-background 'vterm-color-magenta "black")
    (set-face-foreground 'vterm-color-cyan "cyan")
    (set-face-background 'vterm-color-cyan "black")
    (set-face-foreground 'vterm-color-white "white")
    (set-face-background 'vterm-color-white "black")
    (set-face-attribute 'vterm-color-black nil :foreground "#white" :background "#000000"))

  (defface terminal-face
    '((t (:family "Terminus (TTF)" :height 150)))
    "Terminal face for vterm buffers.")
  
  (defun set-vterm-font ()
    
    ;; Устанавливаем желаемый шрифт (если необходимо)
    
    (set (make-local-variable 'buffer-face-mode-face) 'terminal-face)
    (buffer-face-mode t)
    (face-remap-add-relative 'default :background "black" :foreground "white")
    (face-remap-add-relative 'fringe :background "black" :foreground "black")))

;; Таким образом мы получаем всегда чёрный фон терминала, с белым текстом на нём:

;; (defun мой/vterm--get-color-2 (index &rest args)
;;   "Retrieve the color by INDEX from `vterm-color-palette'.

;; A special INDEX of -1 refers to the default colors. ARGS can
;; optionally include `:underline’ or `:inverse-video’ to indicate
;; cells with these attributes. If ARGS contains `:foreground',
;; return the foreground color of the specified face instead of the
;; background color.

;; This function addresses an issue where the foreground color in
;; vterm may match the background color, rendering text invisible."
;;   (let ((foreground    (member :foreground args))
;;        (underline     (member :underline args))
;;        (inverse-video (member :inverse-video args)))
;;     (let* ((fn (if foreground #'face-foreground #'face-background))
;;           (base-face
;;            (cond ((and (>= index 0)
;;                     (< index 16))
;;                  (elt vterm-color-palette index))
;;                 ((and (= index -1) foreground
;;                     underline)
;;                  'vterm-color-underline)
;;                 ((and (= index -1)
;;                     (not foreground)
;;                     inverse-video)
;;                  'vterm-color-inverse-video)
;;                 ((and (= index -2))
;;                  'vterm-color-inverse-video))))
;;       (if base-face
;;           (funcall fn base-face nil t)
;;         (if (and (eq fn 'face-background) (< index 3))
;;             "#000000"
;;           "#ffffff")))))

;; (defun мой/vterm--get-color (index &rest args)
;;   "Get color by INDEX from `vterm-color-palette'.

;; Special INDEX of -1 is used to represent default colors.  ARGS
;; may optionally contain `:underline' or `:inverse-video' for cells
;; with underline or inverse video attribute.  If ARGS contains
;; `:foreground', use foreground color of the respective face
;; instead of background."
;;   (let ((foreground    (member :foreground args))
;;        (underline     (member :underline args))
;;        (inverse-video (member :inverse-video args)))
;;     (funcall (if foreground #'face-foreground #'face-background)
;;           (cond
;;            ((and (>= index 0) (< index 16))
;;             (elt vterm-color-palette index))
;;            ((and (= index -1) foreground underline)
;;             'vterm-color-underline)
;;            ((and (= index -1) (not foreground) inverse-video)
;;             'vterm-color-inverse-video)
;;            (t 'default))
;;           nil 'default)))

;; (advice-add 'vterm--get-color :override #'мой/vterm--get-color-2)

;; (use-package eterm-256color
;;   :defer t
;;   :ensure t)

;; (use-package multi-vterm
;;   :ensure t
;;   :functions (multi-vterm-dedicated-open))

;; (defun открыть-терминал-проекта ()
;;   "Открыть терминал проекта или директории."
;;   (interactive)
;;   (if (eq major-mode 'vterm-mode)
;;       (delete-window)
;;     (let ((окно-терминала (cl-find-if
;;                           (lambda (window)
;;                             (with-current-buffer (window-buffer window) (eq major-mode 'vterm-mode)))
;;                           (window-list))))
;;       (if окно-терминала
;;           (select-window окно-терминала)
;;         (if (projectile-project-p)
;;             (progn
;;               (split-window-below)
;;               (windmove-down)
;;               (projectile-run-vterm))
;;           (multi-vterm-dedicated-open))))))

;;(require 'projectile)

(provide 'про-терминалы)
;;; про-терминалы.el ends here
