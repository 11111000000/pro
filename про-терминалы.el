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
                ("C-c C-t" .#'vterm-copy-mode)
                ("C-q" . #'vterm-send-next-key)
                ("s-v" . #'vterm-yank)
                ("M-p" . (lambda () (interactive) (vterm-send-key "<up>")))
                ("M-n" . (lambda () (interactive) (vterm-send-key "<down>")))))

(use-package multi-vterm
  :ensure t
  :functions (multi-vterm-dedicated-open multi-vterm-dedicated-toggle))

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
  (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm" "changelog-ai.sh" "changelog-ai-new.sh" "ollama" "npm" "nix"))
  :init
  (add-hook 'eshell-mode-hook (lambda ()
                               (progn
                                 (define-key eshell-mode-map "\C-a" 'eshell-bol)))))

(use-package eshell-vterm
  :ensure t
  :after eshell
  :config
  (eshell-vterm-mode))


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

;;;; Автодополнение npm, включая команды из package.json

(require 'eshell)
(require 'json)                       ; в <27: (require 'json)

(defun my/npm-scripts ()
  "Список скриптов из ближайшего package.json."
  (when-let* ((root (locate-dominating-file default-directory "package.json"))
              (file (expand-file-name "package.json" root)))
    (let* ((json-object-type 'alist)  ; если используете json-read-file
           (pkg   (json-read-file file))
           (scr   (alist-get 'scripts pkg)))
      (mapcar #'symbol-name (mapcar #'car scr)))))

;; pcomplete-функция вызывается, когда первая команда — «npm»
(defun pcomplete/npm ()
  "Дополнение для npm в Eshell, включая «npm run <script>»."
  ;; сначала дополняем саму подкоманду npm
  (pcomplete-here*
   '("access" "adduser" "audit" "bugs" "cache" "ci" "completion" "config"
     "dedupe" "deprecate" "doctor" "exec" "explain" "help" "hook" "init"
     "install" "link" "logout" "ls" "outdated" "owner" "pack" "ping"
     "prune" "publish" "rebuild" "restart" "root" "run" "search" "set"
     "star" "start" "stop" "team" "test" "token" "uninstall" "unpublish"
     "update" "version" "view"))
  ;; если уже ввели «run», подсказываем скрипты из package.json
  (when (string= (pcomplete-arg 1) "run")
    (pcomplete-here* (my/npm-scripts))))

(provide 'про-терминалы)
;;; про-терминалы.el ends here
