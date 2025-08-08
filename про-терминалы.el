;;; про-терминалы.el --- Работа с терминалами и Eshell -*- lexical-binding: t -*-
;;
;; Этот файл предоставляет удобную, функциональную и стильную настройку работы с терминалами
;; - vterm: полноценный терминал с яркими сочетаниями клавиш, быстрым копированием и вставкой
;; - multi-vterm: легкое открытие новых вкладок/окон терминала
;; - eshell: интегрированная оболочка Emacs с красивым внешним видом и умным автодополнением
;; - tab-line/tab-bar иконки, продуманное поведение с фокусом и цветами
;; - Красивый промпт и баннер, Git-интеграция, npm-автодополнение, вспомогательные функции
;;
;;; Commentary:
;; Здесь настраиваются терминалы: функции управления окнами, копирования, работы с Eshell,
;; автодополнение команд (в том числе по npm-скриптам), а также оформление — цвета, иконки, табы и промпты.

;;; Code:

;;;; Базовые требования
(require 'установить-из)
(require 'seq)
(require 'eshell)
(require 'vc-git)
(require 'shrink-path)
(require 'all-the-icons)
(require 'esh-mode)
(require 'eshell)
(require 'json)

;;;;= Общие вспомогательные функции =;;;;;

(defun pro/kill-buffer-and-window ()
  "Закрыть текущий буфер и окно, если оно не единственное в этом фрейме."
  (interactive)
  (let ((buf (current-buffer))
        (win (selected-window)))
    (if (one-window-p)
        (kill-buffer buf)
      (progn
        (delete-window win)
        (kill-buffer buf)))))

;;;;= VTerm: современный быстрый терминал =;;;;;

(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank)
  :hook ((vterm-mode . tab-line-mode))
  :bind (:map vterm-mode-map
              ;; Основные бинды для управления терминалом
              ("M-v" . scroll-up-command)
              ("C-\\" . #'toggle-input-method)
              ("C-c C-c" . pro/vterm-interrupt)
              ("C-c C-t" . #'vterm-copy-mode)
              ("C-q" . #'vterm-send-next-key)
              ("C-y" . #'vterm-yank)
              ("s-`" . #'delete-window)
              ("s-v" . #'vterm-yank)
              ;; Перемещение по истории терминала:
              ("M-p" . (lambda () (interactive) (vterm-send-key "<up>")))
              ("M-n" . (lambda () (interactive) (vterm-send-key "<down>")))
              ;; Быстрый переход в copy-mode и навигация по экрану терминала:
              ("C-p" . pro/vterm-line-mode-move-up)
              ("s-q" . kill-current-buffer))
  :config
  ;; В режиме копирования C-g: вернуться к приглашению ввода
  (define-key vterm-copy-mode-map (kbd "C-g") #'pro/vterm-copy-mode-escape)
  ;; Выйти из copy-mode и отправить <up> – прокрутка в истории терминала:
  (define-key vterm-copy-mode-map (kbd "M-p") #'pro/vterm-copy-mode-move-M-up))

(defun pro/vterm-line-mode-move-up ()
  "Войти в `vterm-copy-mode` и подняться на одну строку вверх (или стандартно вызвать `previous-line`)."
  (interactive)
  (unless (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode 1))
  (when (bound-and-true-p vterm-copy-mode)
    (let ((cmd (or (lookup-key vterm-copy-mode-map (kbd "<up>"))
                   (lookup-key vterm-copy-mode-map (kbd "p")))))
      (cond
       (cmd (call-interactively cmd))
       (t (previous-line))))))

(defun pro/vterm-copy-mode-escape ()
  "Выйти из `vterm-copy-mode` и перейти к приглашению терминала."
  (interactive)
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
  (when (and (boundp 'vterm--process-marker) vterm--process-marker)
    (goto-char vterm--process-marker)))

(defun pro/vterm-copy-mode-move-M-up ()
  "Выйти из `vterm-copy-mode` и передать терминалу <up> для листания истории."
  (interactive)
  (when (bound-and-true-p vterm-copy-mode)
    (vterm-copy-mode -1))
  (when (fboundp 'vterm-send-key)
    (vterm-send-key "<up>")))

(defun pro/vterm-interrupt ()
  "Передать явный сигнал прерывания (C-c) в vterm."
  (interactive)
  (when (eq major-mode 'vterm-mode)
    (vterm-send-key "c" nil nil t)))


;;;;= Multi-vterm: несколько вкладок терминала =;;;;;

(use-package multi-vterm
  :ensure t
  :bind (:map vterm-mode-map
              ("s-t" . multi-vterm))
  :functions (multi-vterm-dedicated-open multi-vterm-dedicated-toggle))


;;;;= Eshell — стильно, удобно и мощно =;;;;;

;;= Цветовая схема вкладок для Eshell (см. также про-внешний-вид.el) =;;
(defun pro/eshell-tabline-colors ()
  "Сделать текущую вкладку tab-line в Eshell черной с белым акцентом."
  (face-remap-add-relative 'tab-line-tab-current '(:background "#000000" :foreground "#eeeeee" :weight bold :box nil))
  (face-remap-add-relative 'tab-line-tab '(:background "#000000" :foreground "#cccccc" :weight bold :box nil)))

;;= Темный pop-up Corfu для мини-окон автодополнения в Eshell =;;
(defun pro/eshell-corfu-dark ()
  "Организует темное всплывающее окно Corfu в Eshell."
  (face-remap-add-relative 'corfu-default :background "#181818" :foreground "#eeeeee")
  (face-remap-add-relative 'corfu-current :background "#333333" :foreground "#ffffbb" :weight 'bold)
  (face-remap-add-relative 'corfu-border  :background "#181818"))

;;= Темная терминальная цветовая схема для Eshell =;;
(defun pro/eshell-dark-theme ()
  "Оформляет Eshell: черный фон, терминальные цвета, убирает отступы и поля."
  ;; Удаляем лишние фильтры на всякий случай:
  (setq eshell-output-filter-functions (remove 'ansi-color-process-output eshell-output-filter-functions))
  (setq-local eshell-output-filter-functions (remove 'ansi-color-process-output eshell-output-filter-functions))
  (face-remap-add-relative 'default :background "#000000" :foreground "#cccccc")
  (face-remap-add-relative 'eshell '(:foreground "#eeeeee"))
  (when (facep 'eshell-input)
    (face-remap-add-relative 'eshell-input '(:foreground "#eeeeee")))
  (when (facep 'eshell-syntax-highlighting-builtin-command-face)
    (face-remap-add-relative 'eshell-syntax-highlighting-builtin-command-face '(:foreground "#eeeeee")))
  ;; Локальные терминальные цвета
  (setq-local ansi-color-names-vector
              ["#000000" "#ff5555" "#50fa7b" "#f1fa8c"
               "#bd93f9" "#ff79c6" "#8be9fd" "#bbbbbb"])
  (setq-local ansi-term-color-vector
              [terminal "#000000" "#ff5555" "#50fa7b" "#f1fa8c"
                        "#bd93f9" "#ff79c6" "#8be9fd" "#bbbbbb"])
  ;; Высококонтрастные лица подсветки синтаксиса (если пакет активирован)
  (set-face-attribute 'eshell-syntax-highlighting-alias-face nil
                      :foreground "#bd93f9" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-builtin-command-face nil
                      :foreground "#50fa7b" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-command-substitution-face nil
                      :foreground "#ff79c6" :slant 'italic)
  (set-face-attribute 'eshell-syntax-highlighting-comment-face nil
                      :foreground "#bbbbbb" :slant 'italic)
  (set-face-attribute 'eshell-syntax-highlighting-default-face nil
                      :foreground "#cccccc")
  (set-face-attribute 'eshell-syntax-highlighting-delimiter-face nil
                      :foreground "#f1fa8c")
  (set-face-attribute 'eshell-syntax-highlighting-directory-face nil
                      :foreground "#8be9fd" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-envvar-face nil
                      :foreground "#ff79c6" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-file-arg-face nil
                      :foreground "#eeeeee")
  (set-face-attribute 'eshell-syntax-highlighting-invalid-face nil
                      :foreground "#ff5555" :background "#000000" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-lisp-function-face nil
                      :foreground "#bd93f9")
  (set-face-attribute 'eshell-syntax-highlighting-option-face nil
                      :foreground "#f1fa8c")
  (set-face-attribute 'eshell-syntax-highlighting-shell-command-face nil
                      :foreground "#50fa7b" :weight 'bold)
  (set-face-attribute 'eshell-syntax-highlighting-string-face nil
                      :foreground "#f1fa8c")
  ;; АNSI-цвета для вывода:
  (add-to-list 'eshell-preoutput-filter-functions #'ansi-color-apply)
  ;; Скрываем фринжи и отступы
  (when (get-buffer-window)
    (set-window-fringes (get-buffer-window) 0 0 nil)
    (set-window-margins (get-buffer-window) 0 0))
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq major-mode 'eshell-mode)
                         (get-buffer-window))
                (set-window-fringes (get-buffer-window) 0 0 nil)
                (set-window-margins (get-buffer-window) 0 0)))
            nil t))

(use-package eshell
  :ensure t
  :hook ((eshell-mode . tab-line-mode)
         (eshell-mode . pro/eshell-dark-theme)
         (eshell-mode . pro/eshell-corfu-dark))
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
  (eshell-ask-to-save-history 'always)
  (eshell-visual-commands '("vi" "vim" "screen"
                            "tmux" "top" "htop"
                            "less" "more" "lynx"
                            "links" "ncftp" "mutt"
                            "pine" "tin" "trn" "nmtui" "alsamixer" "mc"
                            "elm" "changelog-ai.sh" "changelog-ai-new.sh"
                            "ollama" "npm" "nix")))

;;= Быстрая Eshell из текущей папки =;;
(defun eshell-here ()
  "Открыть новый буфер Eshell в каталоге текущего буфера."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (eshell t)))

;;= Правильный Backspace: не удалять приглашение =;;
(defun pro/eshell-backspace ()
  "Запретить удаление prompt в Eshell."
  (interactive)
  (if (<= (point) (line-beginning-position))
      (message "Cannot delete after the prompt!")
    (delete-char -1)))

;;= Подключение современных подсказок в Eshell =;;
(use-package eshell-vterm
  :ensure t
  :after eshell
  :config (eshell-vterm-mode))

(use-package eshell-syntax-highlighting
  :init (установить-из :repo "akreisher/eshell-syntax-highlighting")
  :functions (eshell-syntax-highlighting-global-mode)
  :config (eshell-syntax-highlighting-global-mode 1))

(use-package shrink-path
  :ensure t
  :demand t)

;;;;= Современный Eshell Prompt с проектами, git и иконками =;;;;;

(defun приглашение-eshell ()
  "Быстрый и красивый промпт Eshell: иконка+путь+проект+git-ветка+статус."
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
         (git-root (ignore-errors (when (executable-find "git")
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

;;;;= Красивый баннер (welcome-screen) в Eshell =;;;;;

(defun pro/eshell-system-banner-string ()
  "Сформировать баннер с информацией о пользователе/системе для Eshell."
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
     "  " line "\n\n")))

(setq eshell-banner-message '(pro/eshell-system-banner-string))

;;;;= Быстрое переключение с хоть какого окна в Eshell =;;;;;

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 2)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-find-project-root-package 'projectile)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell))

;;;;= Умное автодополнение npm-скриптов для Eshell =;;;;;

(defun pro/npm-scripts ()
  "Вернуть список скриптов из package.json ближайшего проекта."
  (when-let* ((root (locate-dominating-file default-directory "package.json"))
              (file (expand-file-name "package.json" root)))
    (let* ((json-object-type 'alist)
           (pkg (json-read-file file))
           (scr (alist-get 'scripts pkg)))
      (mapcar #'symbol-name (mapcar #'car scr)))))

(defun pcomplete/npm ()
  "Пользовательская функция автодополнения для npm: подсказывать скрипты из package.json."
  (pcomplete-here
   (delete-dups
    (append (list "install" "test" "start" "run" "dev" "build" "version" "help")
            (pro/npm-scripts)))))

(provide 'про-терминалы)

;;; про-терминалы.el ends here
