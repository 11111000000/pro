;;; организация.el --- Конфигурация Org-mode и Outline/Outshine
;;; Commentary:
;;; Code:
;;;; Базовые настройки

(load-library "find-lisp")

(use-package org
  :ensure nil
  
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c o" . org-agenda-open-link))
  :custom ((org-log-done nil)
           (org-agenda-files (find-lisp-find-files "~/Организация" "\.org$"))
           (org-todo-keywords '((sequence "TODO" "ACTIVE" "DONE"))))
  :config
  (require 'org-compat)
  :init)

;;;; Красивые заголовки

(use-package org-bullets
  :if window-system
  :ensure t
  :defer t
  :after (org)
  :hook ((org-mode . org-bullets-mode))

  :custom ((org-bullets-bullet-list '("‣" "‣" ))
           (org-hide-emphasis-markers nil)
           (org-startup-indented t)
           (org-hide-leading-stars nil)
           ))

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

;; PlantUML jar Path

(setq org-plantuml-jar-path (expand-file-name "/nix/store/slmi57xig7mbif52sf757arx5sbj2bni-plantuml-1.2020.15/lib/plantuml.jar"))

;;;; Клавиши

;; Выделение шифтом отключено, т.к. шифт используется для управления статусом

(setq org-support-shift-select nil)

;; Поиск по заголовкам

(define-key org-mode-map (kbd "C-c sh") 'helm-org-in-buffer-headings)

;; Перемещение по заголовкам, со скрытием остальных

(use-package org
  :bind (:map org-mode-map
              (("M-n" . my/org-show-next-heading-tidily)
               ("M-p" . my/org-show-previous-heading-tidily)))
  :init)

;; Перемещение по заголовкам, когда раскрыт только текущий

(defun my/org-show-next-heading-tidily ()
  "Show next entry, keeping other entries closed."
  (interactive)
  (if (save-excursion (end-of-line) (outline-invisible-p))
      (progn (org-show-entry) (show-children))
    (outline-next-heading)
    (unless (and (bolp) (org-on-heading-p))
      (org-up-heading-safe)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))

(defun my/org-show-previous-heading-tidily ()
  "Show previous entry, keeping other entries closed."
  (interactive)
  (let ((pos (point)))
    (outline-previous-heading)
    (unless (and (< (point) pos) (bolp) (org-on-heading-p))
      (goto-char pos)
      (hide-subtree)
      (error "Boundary reached"))
    (org-overview)
    (org-reveal t)
    (org-show-entry)
    (recenter-top-bottom)
    (show-children)
    (recenter-top-bottom)))

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

;;;; TODO Таблицы
;;;; Заметки

(use-package org-noter
  :ensure t
  :bind (
        :map doc-view-mode-map
        ("i" . org-noter-insert-note)))

;;;; Цветные тэги

;; (use-package org-rainbow-tags
;;   ;; :load-path "emacs-lisp/org-rainbow-tags/"
;;   :hook ((org-mode . org-rainbow-tags-mode))
;;   :init)

;;;; Организация кода

(use-package outshine
  :ensure t
  :custom ((outshine-startup-folded-p nil))
  :hook (((prog-mode emacs-lisp-mode js-mode) . outline-minor-mode)
         (outline-minor-mode . outshine-mode)
         (outline-minor-mode . iimage-mode))
  :bind (:map outshine-mode-map
              ("C-<return>" . outshine-insert-heading)
              ("C-<tab>" . outshine-cycle)))

(use-package outshine-bullets
  :init (установить-из-репы :repo "11111000000/outshine-bullets")
  :hook ((outshine-mode . outshine-bullets-mode))
  :custom (
	   (outshine-bullets-bullet-list '("•" "▸" "•" "‣" "•"))
	   )
  )

(provide 'организация)
;;; организация.el ends here
