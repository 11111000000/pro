;;; про-организацию.el --- Конфигурация -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Базовые настройки

(load-library "find-lisp")

(use-package org
  :ensure nil
  :bind (:map org-mode-map
                ("C-<tab>" . org-cycle)
                ("C-TAB" . org-cycle)
                ("C-c o" . org-agenda-open-link)
                ("C-c C-p" . nil))
  :custom ((org-log-done nil)
                                        ;(org-agenda-files (find-lisp-find-files "~/" "\.org$"))
          (org-todo-keywords '((sequence "ОФОРМИТЬ" "СДЕЛАТЬ" "АНАЛИЗ" "ДЕЛЕГИРОВАЛ" "ДЕЛАЮ" "ВОПРОС" "ДЕПЛОЙ" "ГОТОВО"))))
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

;; Скрываем ли блоки кода при открытии документа

(setq-default org-hide-block-startup nil)

;; Включаем ли соответствующую подстветку блоков кода

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
  :ensure t
  :custom ((org-modern-star '("▶" "▷" "▶" "▷" "▶" "▷"))
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
	      (outshine-bullets-bullet-list '("▶" "▷" "▶" "▷" "▶" "▷"))))

;;;; Поддержка диаграмм из блоков на UML

(use-package plantuml-mode
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
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  ;; (setq plantuml-default-exec-mode 'jar)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((plantuml . t))))

(use-package flycheck-plantuml
  :ensure t
  :functions (flycheck-plantuml-setup)
  :after plantuml-mode
  :config (flycheck-plantuml-setup))

(provide 'про-организацию)
;;; про-организацию.el ends here
