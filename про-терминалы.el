;;; про-терминалы.el --- Терминалы -*- lexical-binding: t -*-
;;; Commentary:

;; Конфигурация терминалов

;;; Code:

(require 'установить-из)

(defun my/vterm-copy-mode-move-up ()
  "Включить vterm-copy-mode и сразу перейти на строку выше."
  (interactive)
  (unless (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode 1))
  (when (bound-and-true-p vterm-copy-mode)
    (let ((cmd (or (lookup-key vterm-copy-mode-map (kbd "<up>")) 
                   (lookup-key vterm-copy-mode-map (kbd "p")))))
      (cond
       (cmd (call-interactively cmd))
       ;; если почему-то невозможно определить биндинг,
       ;; стандартная команда "previous-line":
       (t (previous-line))))))

(defun my/vterm-copy-mode-escape ()
  "Выйти из vterm-copy-mode и перейти к приглашению ввода shell."
  (interactive)
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
  (when (and (boundp 'vterm--process-marker) vterm--process-marker)
    (goto-char vterm--process-marker)))

(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command)
                ("C-\\" . #'toggle-input-method)
                ("C-c C-t" . #'vterm-copy-mode)
                ("C-q" . #'vterm-send-next-key)
                ("C-y" . #'vterm-yank)
                ("s-v" . #'vterm-yank)
                ("M-p" . (lambda () (interactive) (vterm-send-key "<up>")))
                ("M-n" . (lambda () (interactive) (vterm-send-key "<down>")))
                ("C-p" . my/vterm-copy-mode-move-up))
  :config
  ;; В режиме копирования C-g возвращает в терминал и переводит к вводу
  (define-key vterm-copy-mode-map (kbd "C-g") #'my/vterm-copy-mode-escape))

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
  :bind (:map eshell-mode-map
         ("C-a" . beginning-of-line)
         ("DEL" . my-eshell-backspace))
  :custom
  (comint-prompt-read-only t)
  (eshell-highlight-prompt nil)
  (eshell-hist-ignoredups t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  (eshell-visual-commands '("vi" "vim" "screen" "tmux" "top" "htop" "less" "more" "lynx" "links" "ncftp" "mutt" "pine" "tin" "trn" "elm" "changelog-ai.sh" "changelog-ai-new.sh" "ollama" "npm" "nix"))
  :config)

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

;; - Этот промпт показывает:
;;   - иконку терминала,
;;   - проект,
;;   - сокращённую директорию (например =~/prj/foo= → =~/…/foo=),
;;   - git-ветку с иконкой и цветовой индикацией если есть изменения,
;;   - ошибку последней команды (или зелёную стрелку если всё ок).
;; - Использует функции: =all-the-icons-octicon=, =all-the-icons-material=, =shrink-path-prompt=, а также Git и проектные функции.

(require 'vc-git)  
(require 'shrink-path)
(require 'all-the-icons) 

(defun приглашение-eshell ()
  "Минималистичный, быстрый и надёжный промпт Eshell с git проектом и статусом."
  (let* ((icons t)
         (default-dir (or (and (stringp default-directory) default-directory) ""))
         (project (when (fboundp 'project-root)
                    (ignore-errors
                      (let ((pr (project-current)))
                        (when pr
                          (file-name-nondirectory
                           (directory-file-name (project-root pr))))))))
         (dir (ignore-errors (shrink-path-prompt default-dir)))
         (path-car (or (and dir (car dir)) ""))
         (path-cdr (or (and dir (cdr dir)) ""))
         (git-root (ignore-errors
                     (when (executable-find "git")
                       (let ((root (vc-git-root default-dir)))
                         (when root (expand-file-name root)))))))
    (let* ((git-branch
            (when git-root
              (ignore-errors
                (let ((branch
                       (car (process-lines "git" "-C" git-root "rev-parse" "--abbrev-ref" "HEAD"))))
                  (unless (or (null branch) (string= branch "HEAD") (string= branch ""))
                    branch)))))
           (git-dirty?
            (when git-root
              (ignore-errors
                (not (string-empty-p
                      (string-trim
                       (shell-command-to-string (format "git -C %s status --porcelain" (shell-quote-argument git-root)))))))))
           (exit-code (if (boundp 'eshell-last-command-status)
                          eshell-last-command-status 0)))
      (concat
       (if icons (all-the-icons-octicon "terminal" :height 1.0) "⎈") " "
       (when project
         (concat
          (if icons (all-the-icons-octicon "repo" :height 0.85 :v-adjust 0) "") " "
          (propertize project 'face 'success) " "))
       (when path-car (propertize path-car 'face 'bold))
       (when path-cdr (propertize path-cdr 'face 'shadow))
       (when (and git-root git-branch)
         (concat " "
                 (if icons (all-the-icons-octicon "git-branch" :height 0.9 :v-adjust 0) "")
                 (propertize (format " %s" git-branch)
                             'face '(:inherit default :weight bold :background unspecified))
                 (when git-dirty?
                   (propertize "*" 'face 'shadow))))
       (if (> exit-code 0)
           (propertize
            (format "\n%s " (if icons (all-the-icons-material "error" :height 0.9 :v-adjust -0.2) "✗"))
            'face 'error)
         (propertize "\n❯ " 'face '(:foreground "#44bb44" :weight bold)))))))

(setq eshell-prompt-function #'приглашение-eshell)

;; (use-package eshell-did-you-mean
;;   :init
;;   (eshell-did-you-mean-setup)
;;   :ensure t)

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
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
