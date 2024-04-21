;;; организация.el --- Конфигурация Org-mode и Outline/Outshine
;;; Commentary:
;;; Code:
;;;; Базовые настройки

(load-library "find-lisp")

(use-package org
  :ensure nil
  :bind (
         :map org-mode-map
         ("C-c o" . org-agenda-open-link)
         ("C-c C-p" . nil))
  :custom ((org-log-done nil)
                                        ;(org-agenda-files (find-lisp-find-files "~/" "\.org$"))
          (org-todo-keywords '((sequence "ОФОРМИТЬ" "СДЕЛАТЬ" "АНАЛИЗ" "ДЕЛАЮ" "ВОПРОС" "ДЕПЛОЙ" "ГОТОВО"))))
  :config
  (require 'org-compat)
  :init)

;;;; Иконки приоритетов

(use-package org-fancy-priorities :ensure t :defer t :hook ((org-mode . org-fancy-priorities-mode)))

;;;; Иконка свёртки

(setq-default org-ellipsis "…")

;;;; Картинки

;; По-умолчанию изображения в Org-файлах показаны:

(setq-default org-startup-with-inline-images nil)

;; Для определения размера отображения, сперва ищем атрибут вида {{

(setq-default org-image-actual-width nil)

;;;; Блоки кода

;; Скрываем блоки кода при открытии документа

(setq-default org-hide-block-startup nil)

;; Включаем соответствующую подстветку

(setq-default org-src-fontify-natively t)

;;  *<Tab>* внутри блоков работает как в режиме блока

(setq-default org-src-tab-acts-natively t)

;; Перед кодом никаких автоотступов


(setq-default org-src-preserve-indentation t
              org-edit-src-content-indentation 0)

;; Авто-обновление картинок при выполнении кода

(defun поправить-встроеные-изображения ()
  "Перерисовать изображения."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

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
   (shell . t)
   ))


;; Выделение шифтом отключено, т.к. шифт используется для управления статусом

(setq org-support-shift-select nil)

;;;; TODO Учёт времени
;;;; Помодоро

;; Простой таймер для учёта рабочего времени и перерывов:

(use-package pomodoro :ensure t :defer t)

(use-package org-pomodoro
  :ensure t :defer t
  :custom (
           (org-pomodoro-length 15)
  	       (org-pomodoro-short-break-length 5)
  	       (org-pomodoro-long-break-length 15)
  	       (org-pomodoro-play-sounds 1)))

;;;; TODO Поли-моды

;; https://polymode.github.io/usage/

;;;; Таблицы

;; Моноширный шрифт для таблиц

(set-face-attribute 'org-table nil :inherit 'fixed-pitch)

;;;; Заметки

(require 'doc-view)

(use-package org-noter
  :ensure t
  :bind (
        :map doc-view-mode-map
        ("i" . org-noter-insert-note)))

;;;; Цветные тэги

(use-package org-rainbow-tags
  :ensure t
  :hook ((org-mode . org-rainbow-tags-mode))
  :init)

;;;; Org modern

;; Современный вид для заголовков и таблиц

(use-package org-modern
  :custom ((org-modern-star '("•" "•" "•" "•" "•"))
          (org-modern-hide-stars " "))
  :ensure t
  :hook ((org-mode . org-modern-mode))
  :init
  
  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t
   org-ellipsis "…"

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  )

;;;; Организация кода

(use-package outshine
  :ensure t
  :defines (outshine-mode-map)
  :custom ((outshine-startup-folded-p nil))
  :hook (((emacs-lisp-mode) . outline-minor-mode)
       (outline-minor-mode . outshine-mode)
       (outline-minor-mode . iimage-mode))
  :bind (:map outshine-mode-map
                ("C-<return>" . outshine-insert-heading)
                ("C-<tab>" . outshine-cycle)))

;; Вместо символов комментария показывать пустоту и уровень вложенности

(use-package outshine-bullets
  :init (установить-из :repo "11111000000/outshine-bullets")
  :hook ((outshine-mode . outshine-bullets-mode))
  :custom (
	        (outshine-bullets-bullet-list '("•" "•" "•" "•" "•"))))

;;;; Поддержка диаграмм из блоков на UML

(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :custom
  ;; (plantuml-jar-path "")
  (org-plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (plantuml-executable-path "/usr/bin/plantuml")
  :config
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(use-package flycheck-plantuml
  :ensure t
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

(use-package eyuml
  :ensure t
  :after org
  :init
  (add-to-list 'org-src-lang-modes '("yuml" . yuml))
  (add-to-list 'org-src-lang-modes '("flowchart-js" . flowchart-js))
  :commands (org-babel-execute:yuml)
  :config
  ;;
  ;; Flowchart.js
  ;;
  (defun org-babel-execute:flowchart-js (body params)
    "Execute a block of flowchartjs code with org-babel."
    (let* ((in-file (org-babel-temp-file "" ".flowchart-js"))
          (out-file (or (cdr (assq :file params))
                        (error "flowchart-js requires a \":file\" header argument")))
          (cmd (format "diagrams flowchart %s %s" in-file out-file))
          (verbosity (or (cdr (assq :verbosity params)) 0)))
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (write-region nil nil in-file))
      (shell-command cmd)
      nil))

  (defun org-babel-execute:yuml (body params)
    "Execute a block of yuml code with org-babel."
    (let ((in-file (org-babel-temp-file "" ".yuml"))
         (type (or (cdr (assq :type params))
                   (error "yuml requires a \":type\" header argument")))
         (out-file (or (cdr (assq :file params))
                       (error "yuml requires a \":file\" header argument")))
         (verbosity (or (cdr (assq :verbosity params)) 0)))
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (while (search-forward "\n" nil t) (replace-match "," nil t))
        (write-region nil nil in-file)
        (message (buffer-substring (point-min) (point-max)))
        (eyuml-create-document type out-file))
      (format "[[file:%s]]" out-file)))

  (defun eyuml-create-document (type &optional out-file)
    "Fetch remote document, TYPE could be class,usecase or activity."
    (let ((out-file (or out-file (eyuml-create-file-name))))
      (request (eyuml-create-url type)
        :parser 'buffer-string
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (when data
                      (with-temp-buffer
                        (set-buffer-file-coding-system 'raw-text)
                        (insert data)
                        (write-region nil nil out-file)))))))))


(provide 'организация)
;;; организация.el ends here
