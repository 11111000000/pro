;;; про-организацию.el --- Конфигурация -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Базовые настройки

(load-library "find-lisp")
(require 'установить-из)

(use-package org
  :bind (:map org-mode-map
                ("C-<tab>" . org-cycle)
                ("C-TAB" . org-cycle)
                ("M-RET" . org-agenda-open-link)
                ("M-p" . org-previous-visible-heading)
                ("M-n" . org-next-visible-heading)
                ("C-c C-p" . nil))
  :custom ((org-log-done nil)
          ;;(org-agenda-files (find-lisp-find-files "~/" "\.org$"))
          (org-todo-keywords '((sequence "TODO" "THINK" "DOING" "|" "DONE")))
          (org-not-done-keywords '("TODO" "THINK" "DOING"))
          (org-done-keywords '("DONE" "CANCEL" "DELEGATED")))
  :config
  (require 'org-compat)
  (require 'org-tempo)
  (setq org-todo-keyword-faces
       '(("TODO" . org-warning)
         ("FIX" . (:foreground "white" :background "red" :weight bold))
         ("IN-PROGRESS" . (:foreground "blue" :weight bold))
         ("DONE" . (:foreground "green" :weight normal))
         ("CANCELLED" . (:foreground "gray" :weight normal))))
  :init)

;;;; Отключаем использование _ и ^ для подстрочных и надстрочных символов

(setq org-use-sub-superscripts nil)

;;;; Иконки приоритетов

(use-package org-fancy-priorities
  :ensure t
  :defer t
  :hook ((org-mode . org-fancy-priorities-mode))
  :custom (org-fancy-priorities-list '((?A . "⚡")
                                 (?B . "⬆")
                                 (?C . "⬇")
                                 (?D . "☕")
                                 (?1 . "⚡")
                                 (?2 . "⮬")
                                 (?3 . "⮮")
                                 )))

;;;; Иконка свёртки

(setq-default org-ellipsis "…")

;;;; Картинки

;; По-умолчанию изображения в Org-файлах показаны:

(setq-default org-startup-with-inline-images t)
(setq-default org-redisplay-inline-images t)

;; Для определения размера отображения, сперва ищем атрибут вида {{

(setq-default org-image-actual-width nil)
(setq-default org-image-max-width 1000)

;;;; Блоки кода

;; Скрываем ли блоки кода при открытии документа

(setq-default org-hide-block-startup nil)

;; Включаем ли соответствующую подстветку блоков кода

(setq-default org-src-fontify-natively t)

;;  *<Tab>* внутри блоков работает как в режиме блока

(setq-default org-src-tab-acts-natively t)

;; Перед кодом никаких автоотступов

(setq-default org-src-preserve-indentation t
         org-edit-src-content-indentation 0)

;; Не автодополнять пару "<>", чтобы вводить быстрые блоки

(require 'elec-pair)
(add-hook 'org-mode-hook (lambda ()
                          (setq-local electric-pair-inhibit-predicate
                                 `(lambda (c)
                                    (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

;; Авто-обновление картинок при выполнении кода

(defun поправить-встроеные-изображения ()
  "Перерисовать изображения."
  
  (org-redisplay-inline-images))

(add-hook 'org-babel-after-execute-hook 'поправить-встроеные-изображения)

;; Открытие блока в окне

(setq-default org-src-window-setup 'current-window)

;; Нет нужды подтверждать выполнение блока (<C-c C-c>)

(setq-default org-confirm-babel-evaluate nil)

;; Активные языки

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)
   (ditaa . t)
   (emacs-lisp . t)
   (plantuml . t)
   (css . t)
   (js . t)
   (R . t)
   ;;(http . t)
   (shell . t)))

;; Выводить C-c C-c текстом (а не таблицей)

(setq org-babel-default-header-args '((:results . "value")))

;; Выделение шифтом отключено, т.к. шифт используется для управления статусом

(setq org-support-shift-select nil)

;; (require 'markdown-mode)

;; (defun render-org-results-as-markdown ()
;;   "Render #+RESULTS: example blocks as markdown preview."
;;   (interactive)
;;   (save-excursion
;;     ;; Ищем все блоки #+RESULTS: с последующим #+begin_example ... #+end_example
;;     (goto-char (point-min))
;;     (while (re-search-forward "^#\\+RESULTS:\\(?: \\(.*\\)\\)?\n#+begin_example\n\\(\\(?:.\\|\n\\)*?\\)#+end_example" nil t)
;;       (let* ((params (match-string 1))
;;             (content (match-string 2))
;;             (begin (match-beginning 0))
;;             (end (match-end 0))
;;             (rendered-content))
;;         ;; Используем markdown-mode для рендеринга содержимого
;;         (with-temp-buffer
;;           (insert content)
;;           (markdown)
;;           (setq rendered-content (buffer-string)))
;;         ;; Создаем оверлей для отображения отрендеренного контента
;;         (let ((o (ov begin end)))
;;           (ov-set o 'display rendered-content)
;;           (ov-set o 'ov-rendered t))))))

;; ;; Функция для обновления рендеринга при изменениях
;; (defun update-org-results-as-markdown ()
;;   "Update markdown rendering in #+RESULTS: blocks."
;;   (when (eq major-mode 'org-mode)
;;     (render-org-results-as-markdown)))

;; ;; Добавляем хук для автоматического обновления рендеринга при сохранении файла
;; (add-hook 'after-save-hook 'update-org-results-as-markdown)

;;;; Помодоро

;; Простой таймер для учёта рабочего времени и перерывов:

(use-package pomodoro :ensure t :defer t)

(use-package org-pomodoro
  :defer t
  :ensure t :defer t
  :custom (
          (org-pomodoro-length 15)
  	      (org-pomodoro-short-break-length 5)
  	      (org-pomodoro-long-break-length 15)
  	      (org-pomodoro-play-sounds 1)))

;;;; Поли-моды

;; https://polymode.github.io/usage/

;;;; Таблицы

;; Моноширный шрифт для таблиц

(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

;;;; Заметки

(require 'doc-view)

(use-package org-noter
  :defer t
  :ensure t
  :bind (
         :map doc-view-mode-map
         ("i" . org-noter-insert-note)))

;;;; Цветные тэги

(use-package org-rainbow-tags
  :defer t
  :ensure t
  :hook ((org-mode . org-rainbow-tags-mode))
  :init)

;;;; Org modern

;; Современный вид для заголовков и таблиц

(use-package org-modern
  :defer t
  :if window-system
  :ensure t
  :custom ((org-modern-star '("●" "▶" "◆" "▷" "□" "◍"))
          (org-modern-hide-stars " "))
  :hook ((org-mode . org-modern-mode))
  :init
  
  (setq
   org-auto-align-tags nil
   org-tags-column 0
   org-fold-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"))

;;;; Организация кода - аутлайнер для любых файлов

(use-package outshine
  :ensure t
  :defines (outshine-mode-map)
  :custom ((outshine-startup-folded-p nil))
  :hook (((emacs-lisp-mode) . outline-minor-mode)
       (outline-minor-mode . outshine-mode)
       (outline-minor-mode . iimage-mode))
  :bind (:map outshine-mode-map
                ("C-<return>" . outshine-insert-heading)
                ("C-M-i" . nil)))

;; В коде, вместо символов комментария показывать уровень вложенности

(use-package outshine-bullets
  :defer t
  :init (установить-из :repo "11111000000/outshine-bullets")
  :hook ((outshine-mode . outshine-bullets-mode))
  :custom (
	      (outshine-bullets-bullet-list '("●" "▶" "▷" "□" "◆" "◍"))))

(set-face-attribute 'org-hide nil :foreground (face-background 'default))

;;;; Поддержка диаграмм из блоков на UML

(use-package plantuml-mode
  :defer t
  :ensure t
  :mode "\\.plantuml\\'"
  :custom
  ;; (plantuml-jar-path "")
  ;; (org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  ;; (plantuml-executable-path "/usr/bin/plantuml")
  (plantuml-default-exec-mode 'jar)
  (plantuml-jar-path "/usr/share/plantuml/plantuml.jar")
  (org-plantuml-jar-path (expand-file-name "/usr/share/plantuml/plantuml.jar"))
  :config
  
  ;; Понимать блоки кода UML
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (setq plantuml-default-exec-mode 'jar)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t)))

  ;; Быстрый ввод блоков кода UML
  (add-to-list 'org-structure-template-alist
             '("uml" . "src plantuml :file ./diagram.svg")))

;;;; Поддержка Mermaid

(use-package ob-mermaid
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(mermaid . t)))

;;;; Асинхронное выполнение блоков кода

(use-package ob-async
  :ensure t)

;;;; Канбан

(use-package kanban :ensure t)
(use-package org-kanban :ensure t)

;;;; Полезные функции

(require 'org)
(require 'ox)
(require 'outline)

(defun my/org-archive-done-tasks ()
  "Вырезает все записи со статусом DONE и сохраняет их в архивный файл."
  (interactive)
  (let* ((current-file (buffer-file-name))
        (archive-file (concat current-file ".archive.org"))
        (done-tasks '()))
    (save-excursion
      ;; Сначала ищем записи со статусом DONE
      (goto-char (point-min))
      (while (re-search-forward "^\\*+[ ]+DONE" nil t)
        (let ((start (match-beginning 0))
             (end (progn
                    (outline-next-heading)
                    (point))))
          (push (buffer-substring-no-properties start end) done-tasks)
          (delete-region start end)))
      ;; Если есть записи, сохраняем их в архивный файл
      (when done-tasks
        (with-temp-buffer
          (insert (mapconcat 'identity (nreverse done-tasks) "\n\n"))
          (append-to-file (point-min) (point-max) archive-file)))
      ;; Выводим сообщение об успешном архивировании
      (if done-tasks
          (message "Делания с пометкой DONE были перенесены в %s" archive-file)
        (message "Не найдено задач с пометкой DONE.")))))

(defun org-region-to-markdown (begin end)
  "Преобразует выделенный регион BEGIN до END из Org-mode в Markdown и заменяет его."
  (interactive "r")
  (let* ((org-content (buffer-substring-no-properties begin end))
         (markdown-content (org-export-string-as org-content 'md t nil)))
    (delete-region begin end)
    (insert markdown-content)))

(defun pro/org-export-to-html-and-view ()
  "Export the current Org buffer to HTML, then view it in eww."
  (interactive)
  ;; Make sure we have a file-backed buffer and the latest changes are saved
  (unless buffer-file-name
    (error "Buffer is not visiting a file"))
  (save-buffer)
  ;; Export to HTML; `org-export-to-file' returns the full path
  (let* ((org-export-with-broken-links t)   ; optional: suppress link warnings
         (html-file (org-export-to-file
                      'html
                      (concat (file-name-sans-extension buffer-file-name) ".html"))))
    (eww-open-file html-file)))

(provide 'про-организацию)
;;; про-организацию.el ends here
