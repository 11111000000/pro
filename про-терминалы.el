;;; про-терминалы.el --- Терминалы -*- lexical-binding: t -*-
;;; Commentary:

;; Конфигурация терминалов

;;; Code:

(require 'установить-из)

(defun pro/kill-buffer-and-window ()
  "Закрыть текущий буфер и окно, если оно не единственное."
  (interactive)
  (let ((buf (current-buffer))
        (win (selected-window)))
    (if (one-window-p)
        (kill-buffer buf)
      (progn
        (delete-window win)
        (kill-buffer buf)))))

(defun pro/vterm-line-mode-move-up ()
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

(defun pro/vterm-copy-mode-escape ()
  "Выйти из vterm-copy-mode и перейти к приглашению ввода shell."
  (interactive)
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
  (when (and (boundp 'vterm--process-marker) vterm--process-marker)
    (goto-char vterm--process-marker)))

(defun pro/vterm-copy-mode-move-M-up ()
  "Выйти из vterm-copy-mode и отправить терминалу <up> (Meta-p в copy-mode делает перемещение в истории терминала)."
  (interactive)
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
  (when (fboundp 'vterm-send-key)
    (vterm-send-key "<up>")))

(defun pro/vterm-interrupt ()
  "Send C-c as an interrupt in vterm, always."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (vterm-send-key "c" nil nil t)))


(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :hook ((vterm-mode . tab-line-mode))
  :bind (:map vterm-mode-map
                ("M-v" . scroll-up-command)
                ("C-\\" . #'toggle-input-method)
                ("C-c C-c" . pro/vterm-interrupt)
                ("C-c C-t" . #'vterm-copy-mode)
                ("C-q" . #'vterm-send-next-key)
                ("C-y" . #'vterm-yank)
                ("s-`" . #'delete-window)
                ("s-v" . #'vterm-yank)
                ("M-p" . (lambda () (interactive) (vterm-send-key "<up>")))
                ("M-n" . (lambda () (interactive) (vterm-send-key "<down>")))
                ("C-p" . pro/vterm-line-mode-move-up)
                ;; "M-p" для vterm-copy-mode настроим в :config ниже!
                ("s-q" . kill-current-buffer))
  :config
  ;; В режиме копирования C-g возвращает в терминал и переводит к вводу
  (define-key vterm-copy-mode-map (kbd "C-g") #'pro/vterm-copy-mode-escape)
  ;; В режиме copy-mode: M-p — вернуться в терминал и послать <up>
  (define-key vterm-copy-mode-map (kbd "M-p") #'pro/vterm-copy-mode-move-M-up))

(use-package multi-vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("s-t" . multi-vterm))
  :functions (multi-vterm-dedicated-open multi-vterm-dedicated-toggle))

(require 'seq)
(require 'eshell)

; (use-package capf-autosuggest }
;   :ensure t }
;   :hook }
;   (eshell-mode capf-autosuggest-mode) }
;    (comint-mode capf-autosuggest-mode)) }

;; Цветовая схема tab-line специально для Eshell

;; (defun pro/eshell-tabline-colors ()
;;   "Меняет только текущую вкладку tab-line в Eshell: чёрный фон, белый текст, жирный."
;;   (face-remap-add-relative 'tab-line-tab-current '(:background "#000000" :foreground "#eeeeee" :weight bold :box nil))
;;   (face-remap-add-relative 'tab-line-tab '(:background "#000000" :foreground "#cccccc" :weight bold :box nil))
;;   )

;; Оболочка Emacs Shell
(defun pro/eshell-corfu-dark ()
  "Dark popup for corfu in Eshell."
  (face-remap-add-relative 'corfu-default
                           :background "#181818" :foreground "#eeeeee")
  (face-remap-add-relative 'corfu-current
                           :background "#333333" :foreground "#ffffbb" :weight 'bold)
  (face-remap-add-relative 'corfu-border
                           :background "#181818"))



(defun pro/eshell-dark-theme ()
  "Сделать буфер Eshell максимально похожим на терминал: чёрный фон, терминальные ansi-цвета, без fringes."
  ;; Очистить ansi-color-process-output из ГЛОБАЛЬНОГО и buffer-local фильтров, если где-либо была добавлена:  
  (setq eshell-output-filter-functions
        (remove 'ansi-color-process-output eshell-output-filter-functions))
  (setq-local eshell-output-filter-functions
              (remove 'ansi-color-process-output eshell-output-filter-functions))
  ;; Черный фон и светлый текст (face-remap default и eshell faces для более яркого ввода)
  (face-remap-add-relative 'default :background "#000000" :foreground "#cccccc")
  (face-remap-add-relative 'eshell '(:foreground "#eeeeee"))
  (when (facep 'eshell-input)
    (face-remap-add-relative 'eshell-input '(:foreground "#eeeeee")))
  ;; Сделать команду тоже ярко-серой (если face существует в данной версии)
  (when (facep 'eshell-syntax-highlighting-builtin-command-face)
    (face-remap-add-relative 'eshell-syntax-highlighting-builtin-command-face '(:foreground "#eeeeee")))
  ;; Настраиваем ansi/term-colors локально для терминального эффекта
  (setq-local ansi-color-names-vector
              ["#000000" "#ff5555" "#50fa7b" "#f1fa8c"
               "#bd93f9" "#ff79c6" "#8be9fd" "#bbbbbb"])
  (setq-local ansi-term-color-vector
              [terminal "#000000" "#ff5555" "#50fa7b" "#f1fa8c"
                        "#bd93f9" "#ff79c6" "#8be9fd" "#bbbbbb"])

  ;; Явно задаём цвета для eshell-syntax-highlighting (чтобы соответствовали терминальным цветам).
  (set-face-attribute 'eshell-syntax-highlighting-alias-face nil
                      :foreground "#bd93f9" :weight 'bold) ; яркий синий
  (set-face-attribute 'eshell-syntax-highlighting-builtin-command-face nil
                      :foreground "#50fa7b" :weight 'bold) ; зелёный
  (set-face-attribute 'eshell-syntax-highlighting-command-substitution-face nil
                      :foreground "#ff79c6" :slant 'italic) ; пурпурный курсив
  (set-face-attribute 'eshell-syntax-highlighting-comment-face nil
                      :foreground "#bbbbbb" :slant 'italic) ; светло-серый
  (set-face-attribute 'eshell-syntax-highlighting-default-face nil
                      :foreground "#cccccc") ; дефолт (чуточку светлее для команд)
  (set-face-attribute 'eshell-syntax-highlighting-delimiter-face nil
                      :foreground "#f1fa8c") ; жёлтый
  (set-face-attribute 'eshell-syntax-highlighting-directory-face nil
                      :foreground "#8be9fd" :weight 'bold) ; бирюзовый (голубой)
  (set-face-attribute 'eshell-syntax-highlighting-envvar-face nil
                      :foreground "#ff79c6" :weight 'bold) ; пурпурный
  (set-face-attribute 'eshell-syntax-highlighting-file-arg-face nil
                      :foreground "#eeeeee") ; почти белый
  (set-face-attribute 'eshell-syntax-highlighting-invalid-face nil
                      :foreground "#ff5555" :background "#000000" :weight 'bold) ; ярко-красный
  (set-face-attribute 'eshell-syntax-highlighting-lisp-function-face nil
                      :foreground "#bd93f9") ; синий/фиолетовый
  (set-face-attribute 'eshell-syntax-highlighting-option-face nil
                      :foreground "#f1fa8c") ; жёлтый
  (set-face-attribute 'eshell-syntax-highlighting-shell-command-face nil
                      :foreground "#50fa7b" :weight 'bold) ; зелёный для команд
  (set-face-attribute 'eshell-syntax-highlighting-string-face nil
                      :foreground "#f1fa8c") ; жёлтые строки

  (add-to-list 'eshell-preoutput-filter-functions #'ansi-color-apply)

  (when (get-buffer-window)
    (set-window-fringes (get-buffer-window) 0 0 nil)
    (set-window-margins (get-buffer-window) 0 0))
  
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq major-mode 'eshell-mode)
                         (get-buffer-window))
                (set-window-fringes (get-buffer-window) 0 0 nil)
                (set-window-margins (get-buffer-window) 0 0)))
            nil t)
  
  ;; (add-hook 'window-configuration-change-hook
  ;;           (lambda ()
  ;;             (when (eq major-mode 'eshell-mode)
  ;;               (set-window-fringes (get-buffer-window) 0 0 nil)
  ;;               (set-window-margins (get-buffer-window) 0 0)))
  ;;           nil t)
  )

(require 'esh-mode)

(use-package eshell
  :ensure t
  :hook ((eshell-mode . tab-line-mode)
         (eshell-mode . pro/eshell-dark-theme)
         (eshell-mode . pro/eshell-corfu-dark)
         ;; (eshell-mode . pro/eshell-tabline-colors)
         )
  :bind (:map eshell-mode-map
         ("C-a" . beginning-of-line)
         ("DEL" . pro/eshell-backspace)
         ("s-q" . pro/kill-buffer-and-window)
         ("s-t" . eshell-here))
  :custom
  (comint-prompt-read-only t)
  (eshell-highlight-prompt nil)
  (eshell-hist-ignoredups t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  (eshell-visual-commands '("vi" "vim" "screen"
                            "tmux" "top" "htop"
                            "less" "more" "lynx"
                            "links" "ncftp" "mutt"
                            "pine" "tin" "trn"
                            "elm" "changelog-ai.sh" "changelog-ai-new.sh"
                            "ollama" "npm" "nix"))
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


(defun pro/eshell-backspace ()
  "Prevent Backspace from deleting if the cursor is after the prompt."
  (interactive)
  (if
      (<= (point) (line-beginning-position))
      ;; If the point is at or after the prompt, do nothing
      (message "Cannot delete after the prompt!")
    ;; Otherwise, perform the normal backspace operation
    (delete-char -1)))


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
       " " (if icons (all-the-icons-octicon "terminal" :height 1.0) "⎈") " "
       (when path-car (propertize path-car 'face 'bold))
       (when path-cdr (propertize path-cdr 'face 'default))
       (when project
         (concat
          " "
          (if icons (all-the-icons-octicon "repo" :height 0.85 :v-adjust 0) "")
          " "
          (propertize project 'face 'success)))
       (when (and git-root git-branch)
         (concat " "
                 (if icons (all-the-icons-octicon "git-branch" :height 0.9 :v-adjust 0) "")
                 " "
                 (propertize (format "%s" git-branch)
                             'face 'font-lock-type-face)
                 (when git-dirty?
                   (propertize "*" 'face 'default))))
       (let ((prompt-color (if (> exit-code 0) "#bb7744" "#44bb44")))
         (set-face-foreground 'eshell-prompt prompt-color)
         (set-face-attribute 'eshell-prompt nil :weight 'bold)
         (concat
          "\n"
          (propertize
           " ❯ "
           'face 'eshell-prompt)))))))

(setq eshell-prompt-function #'приглашение-eshell)

;; (use-package eshell-did-you-mean
;;   :init
;;   (eshell-did-you-mean-setup)
;;   :ensure t)

;;;; Красивый баннер для Eshell со сведениями о системе

;; Функция формирует баннер (ВОЗВРАЩАЕТ СТРОКУ)
(defun pro/eshell-system-banner-string ()
  "Вернуть красивый баннер с информацией о системе для вывода в Eshell."
  (let* ((user (user-login-name))
         (host (system-name))
         (os   (capitalize (symbol-name system-type)))
         (emacs-version-string (format "Emacs %s" emacs-version))
         (time (format-time-string "%Y-%m-%d %H:%M:%S"))
         (line (make-string 58 ?─)))
    (concat
     "\n"
     (format "  👤 %s  ⭐ %s  💻 %s  ⏰ %s\n" user host os time)
     (format "  %s\n" emacs-version-string)
     "  " line "\n\n"
     )))

;; Настраиваем переменную, как это ожидает модуль em-banner
(setq eshell-banner-message '(pro/eshell-system-banner-string))

;; Не требуется отдельная функция eshell-banner-message – eshell сам вызывает функцию из переменной

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 2)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-find-project-root-package 'projectile)
                                        ;(eshell-toggle-find-project-root-package t)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell))


;;;; Автодополнение npm, включая команды из package.json

(require 'eshell)
(require 'json)                       ; в <27: (require 'json)

(defun pro/npm-scripts ()
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
    (pcomplete-here* (pro/npm-scripts))))

(provide 'про-терминалы)
;;; про-терминалы.el ends here
