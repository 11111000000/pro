;;; про-терминалы.el --- Терминалы -*- lexical-binding: t -*-
;;; Commentary:

;; Конфигурация терминалов

;;; Code:

(require 'установить-из)

(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command)
                ("C-\\" . #'toggle-input-method)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank)))

; (use-package capf-autosuggest }
;   :ensure t }
;   :hook }
;   (eshell-mode capf-autosuggest-mode) }
;    (comint-mode capf-autosuggest-mode)) }

;; Оболочка Emacs Shell

(use-package eshell
  :ensure t
  :defines (eshell-mode-map)
  :hook (eshell-mode . tab-line-mode)
  :custom
  (comint-prompt-read-only t)
  ;;(eshell-prompt-function 'приглашение-eshell)
  (eshell-highlight-prompt nil)
  (eshell-hist-ignoredups t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  ;;(eshell-prompt-regexp "❯❯❯ ")
  (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm" "changelog-ai.sh" "changelog-ai-new.sh" "ollama" "docker"))
  :init
  (add-hook 'eshell-mode-hook (lambda ()
                               (progn
                                 (define-key eshell-mode-map "\C-a" 'eshell-bol)
                                 (define-key eshell-mode-map [up] 'previous-line)
                                 (define-key eshell-mode-map [down] 'next-line)))))

(defun eshell-here ()
  "Открыть новый буфер Eshell в каталоге текущего буфера."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (eshell t)))

;; Подсветка синтаксиса в Eshell

(use-package eshell-syntax-highlighting
  :init (установить-из :repo "akreisher/eshell-syntax-highlighting")
  :functions (eshell-syntax-highlighting-global-mode)
  :config
  (eshell-syntax-highlighting-global-mode 1))

;;Предпросмотр а-ля в Plan9

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

;; (defun приглашение-eshell ()
;;   "Настройка приглашения оболочки EShell."
;;   (let* ((git-branch-unparsed
;;           (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
;;          (git-branch
;;           (if (string= git-branch-unparsed "")
;;               ""
;;             (substring git-branch-unparsed 0 -1)))
;;          (shrunk-path (shrink-path-prompt default-directory))
;;          (path-car (or (car shrunk-path) ""))
;;          (path-cdr (or (cdr shrunk-path) "")))
;;     (format "%s %s%s %s\n%s "
;;             (all-the-icons-octicon "repo")
;;             path-car
;;             path-cdr
;;             (if (string= git-branch "")
;;                 ""
;;               (propertize (concat "[" git-branch "]") 'face '(:inherit font-lock-string-face)))
;;             (propertize "❯❯❯" 'face '(:foreground "#33aa33")))))
;; (use-package eshell-did-you-mean
;;   :init
;;   (eshell-did-you-mean-setup)
;;   :ensure t)

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 4)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-find-project-root-package 'projectile)
                                        ;(eshell-toggle-find-project-root-package t)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell))

(defun my-eshell-backspace ()
  "Prevent Backspace from deleting if the cursor is after the prompt."
  (interactive)
  (if
      (<= (point) (line-beginning-position))
      ;; If the point is at or after the prompt, do nothing
      (message "Cannot delete after the prompt!")
    ;; Otherwise, perform the normal backspace operation
    (delete-char -1)))

(define-key eshell-mode-map (kbd "DEL") 'my-eshell-backspace)

(provide 'про-терминалы)
;;; про-терминалы.el ends here
