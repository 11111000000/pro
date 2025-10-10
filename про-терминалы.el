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
  "Закрыть текущий буфер и окно, если оно не единственное в этом фрейме.
Если это терминальный буфер (vterm, term, shell), убивать процесс без подтверждения."
  (interactive)
  (let ((buf (current-buffer))
        (win (selected-window)))
    (when (memq major-mode '(vterm-mode term-mode shell-mode))
      (let ((kill-buffer-query-functions
             (remq 'process-kill-buffer-query-function kill-buffer-query-functions)))
        ;; Удаляем все kill-buffer-query, чтобы не было "Kill terminal ...?".
        (set (make-local-variable 'kill-buffer-query-functions) nil)))
    (if (one-window-p)
        (kill-buffer buf)
      (progn
        (delete-window win)
        (kill-buffer buf)))))

;;;;= VTerm: современный быстрый терминал =;;;;;

(use-package vterm
  :ensure t
  :functions (vterm-send-next-key vterm-yank vterm-send-string)
  :hook ((vterm-mode . tab-line-mode))
  :bind (:map vterm-mode-map
              ;; Основные бинды для управления терминалом
              ("M-v" . scroll-up-command)
              ("C-\\" . #'toggle-input-method)
              ("C-c C-c" . pro/vterm-interrupt)
              ("C-c C-t" . #'vterm-copy-mode)
              ("C-q" . #'vterm-send-next-key)
              ;; Вставка из kill-ring в vterm с корректной поддержкой yank-pop:
              ("C-y" . pro/vterm-yank)
              ("C-x y" . pro/vterm-consult-yank-pop)
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

(defun pro/vterm-yank (&optional arg)
  "Вставить строку из kill-ring в vterm, корректно поддерживая последующий M-y.
С префиксом ARG вставляет не последнюю, а ARG-ю запись (как в `current-kill')."
  (interactive "P")
  (let* ((n (if (integerp arg) arg 0))
         (str (current-kill n t)))
    (when (and (eq major-mode 'vterm-mode) str)
      ;; Помечаем действие как yank, чтобы работал yank-pop/consult-yank-pop
      (setq this-command 'yank)
      ;; Отправляем как bracketed paste для корректной многострочной вставки
      (vterm-send-string str t))))

(defun pro/vterm-consult-yank-pop ()
  "Открыть меню Consult kill-ring и вставить выбранную строку в vterm.
Работает и в char-mode: отправляет в терминал, не редактируя буфер Emacs."
  (interactive)
  (require 'consult)
  ;; Используем внутреннюю функцию Consult для получения строки без вставки в буфер
  (let ((str (consult--read-from-kill-ring)))
    (when (and (eq major-mode 'vterm-mode) str)
      (setq this-command 'yank)
      (vterm-send-string str t))))


;; Фикс: после выхода из vterm-copy-mode иногда в kill-ring попадает пустая запись.
;; Удаляем пустую "голову" kill-ring и корректируем указатель `kill-ring-yank-pointer'.
(defun pro/vterm--string-empty-or-ws-p (s)
  (or (null s) (string-match-p "\\`[[:space:]]*\\'" s)))

;; Локальный флаг: было ли реальное копирование во время vterm-copy-mode.
(defvar-local pro/vterm--copied-in-copy-mode nil)

(defun pro/vterm--on-enter-copy-mode (&rest _)
  "Сбросить флаг о копировании при входе в `vterm-copy-mode'."
  (when (and (eq major-mode 'vterm-mode)
             (bound-and-true-p vterm-copy-mode))
    (setq pro/vterm--copied-in-copy-mode nil)))

(defun pro/vterm--flag-copied-in-copy-mode (&rest _)
  "Пометить, что копирование произошло, если мы в `vterm-copy-mode' vterm."
  (when (and (eq major-mode 'vterm-mode)
             (bound-and-true-p vterm-copy-mode))
    (setq pro/vterm--copied-in-copy-mode t)))

(defun pro/vterm--fix-kill-ring-after-copy-mode (&rest _)
  "Если вышли из `vterm-copy-mode', почистить пустые элементы и при необходимости
откатить указатель `kill-ring-yank-pointer' на предыдущую запись."
  (when (and (eq major-mode 'vterm-mode)
             (not (bound-and-true-p vterm-copy-mode)))
    ;; 1) Сначала убираем пустые головы kill-ring.
    (let ((moved nil))
      (while (and kill-ring (pro/vterm--string-empty-or-ws-p (car kill-ring)))
        (setq kill-ring (cdr kill-ring))
        (setq moved t))
      (when (and (listp kill-ring-yank-pointer)
                 (or (null kill-ring-yank-pointer)
                     (pro/vterm--string-empty-or-ws-p (car kill-ring-yank-pointer))
                     moved))
        (setq kill-ring-yank-pointer kill-ring)))
    ;; 2) Если в copy-mode ничего не копировали — перескочить новый «шумный» верхний элемент.
    (unless pro/vterm--copied-in-copy-mode
      (when (and kill-ring (cdr kill-ring))
        ;; Не трогаем сам kill-ring, только переносим указатель, чтобы C-y/M-y/consult брали «предыдущее».
        (setq kill-ring-yank-pointer (cdr kill-ring))))
    ;; Сброс флага на всякий случай.
    (setq pro/vterm--copied-in-copy-mode nil)))

;; Глобальный хук: помечаем факт копирования (через kill-new) внутри vterm-copy-mode.
(advice-add 'kill-new :after #'pro/vterm--flag-copied-in-copy-mode)

;; Запускаем фиксацию и управление указателем при переключении copy-mode.
(with-eval-after-load 'vterm
  (advice-add 'vterm-copy-mode :before #'pro/vterm--on-enter-copy-mode)
  (advice-add 'vterm-copy-mode :after  #'pro/vterm--fix-kill-ring-after-copy-mode))

;;;;= Multi-vterm: несколько вкладок терминала =;;;;;

(use-package multi-vterm
  :ensure t
  :bind (
         ;; :map vterm-mode-map
         ;; ("s-t" . multi-vterm)
         )
  :functions (multi-vterm-dedicated-open multi-vterm-dedicated-toggle))


;;;;= Eshell — стильно, удобно и мощно =;;;;;

;;= Цветовая схема вкладок для Eshell (см. также про-внешний-вид.el) =;;
;; (defun pro/eshell-tabline-colors ()
;;   "Сделать вкладку Eshell в tab-line всегда чёрной: активную и при потере фокуса."
;;   ;; Только tab-line лица — не трогаем pro-tabs-*, чтобы не влиять на tab-bar.
;;   (face-remap-add-relative 'tab-line-tab-current '(:background "#000000" :foreground "#eeeeee" :weight bold :box nil))
;;   ;; В неактивном окне текущая вкладка использует tab-line-tab:
;;   (face-remap-add-relative 'tab-line-tab '(:background "#000000" :foreground "#eeeeee" :weight bold :box nil)))

(defface pro/eshell-tabline-black-face
  '((t (:background "#000000")))
  "Однотонный чёрный фон для волны pro-tabs в Eshell.")

(defun pro/eshell-tabline-name-function (buffer &optional _buffers)
  "Tab-line formatter для Eshell: активная вкладка и её волны всегда чёрные.
Центральная часть вкладки остаётся чёрной даже при потере фокуса."
  (let* ((current? (eq buffer (window-buffer)))
         (eshell? (with-current-buffer buffer (eq major-mode 'eshell-mode)))
         (baseface (if current? 'pro-tabs-active-face 'pro-tabs-inactive-face))
         ;; Чёрный фон только для текущей вкладки Eshell; неактивные остаются обычными.
         (face (if (and eshell? current?)
                   (list 'pro/eshell-tabline-black-face baseface)
                 baseface))
         (h pro-tabs-tab-line-height)
         (icon (when (fboundp 'pro-tabs--icon) (pro-tabs--icon buffer 'tab-line)))
         (wave-l (propertize " " 'display
                             (if (and eshell? current?)
                                 (pro-tabs--wave-right 'tab-line 'pro/eshell-tabline-black-face  (+ 1 h))
                               (pro-tabs--wave-right baseface 'tab-line (+ 1 h)))))
         (wave-r (propertize " " 'display
                             (if (and eshell? current?)
                                 (pro-tabs--wave-left 'pro/eshell-tabline-black-face 'tab-line (+ 1 h))
                               (pro-tabs--wave-left 'tab-line baseface (+ 1 h)))))
         (txt (concat wave-l (or icon "") " " (buffer-name buffer) wave-r)))
    ;; Препендим лицо, чтобы наш чёрный не перебивался tab-line-tab/… лицами.
    (add-face-text-property 0 (length txt) face nil txt)
    txt))

(defun pro/eshell-tabline-black-waves-setup ()
  "Сделать чёрными боковые волны активной вкладки tab-line только в Eshell."
  (setq-local tab-line-tab-name-function #'pro/eshell-tabline-name-function))

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

  ;; Фиксированные цвета для вывода eshell/ls, не зависящие от темы (на чёрном фоне)
  (setq-local eshell-ls-use-colors t)
  (dolist (pair '((eshell-ls-directory  . (:foreground "#8be9fd" :weight bold))
                  (eshell-ls-symlink    . (:foreground "#bd93f9" :slant italic))
                  (eshell-ls-executable . (:foreground "#50fa7b" :weight bold))
                  (eshell-ls-archive    . (:foreground "#ff79c6"))
                  (eshell-ls-backup     . (:foreground "#777777"))
                  (eshell-ls-clutter    . (:foreground "#777777"))
                  (eshell-ls-missing    . (:foreground "#ff5555" :weight bold))
                  (eshell-ls-product    . (:foreground "#f1fa8c"))
                  (eshell-ls-readonly   . (:foreground "#bbbbbb"))
                  (eshell-ls-special    . (:foreground "#f1fa8c" :weight bold))
                  (eshell-ls-unreadable . (:foreground "#ff5555"))))
    (let ((face (car pair))
          (spec (cdr pair)))
      (when (facep face)
        (apply #'face-remap-add-relative face spec))))

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
  ;; Фринжи: чёрный фон и стандартная ширина; сохраняем нулевые margins.
  (face-remap-add-relative 'fringe '(:background "#000000"))
  (when (get-buffer-window)
    (set-window-fringes (get-buffer-window) nil nil nil)
    (set-window-margins (get-buffer-window) 0 0))
  (add-hook 'window-configuration-change-hook
            (lambda ()
              (when (and (eq major-mode 'eshell-mode)
                         (get-buffer-window))
                (set-window-fringes (get-buffer-window) nil nil nil)
                (set-window-margins (get-buffer-window) 0 0)))
            nil t))

(use-package eshell
  :ensure t
  :hook ((eshell-mode . tab-line-mode)
         (eshell-mode . pro/eshell-dark-theme)
         (eshell-mode . pro/eshell-corfu-dark)
         (eshell-mode . pro/eshell-tabline-colors)
         (eshell-mode . pro/eshell-tabline-black-waves-setup))
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
