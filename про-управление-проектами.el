;;; про-управление-проектами.el --- Управление проектами -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Модуль для работы с проектами

(use-package projectile
  :defer t 
  :ensure t
  :defines (projectile-command-map)
  :custom ((projectile-sort-order 'recently-active)
                                        ;(projectile-project-search-path '("~/Проекты/"))
          (projectile-switch-project-action #'projectile-dired))
  :bind (:map projectile-command-map
                ("ss" . consult-grep))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode t))

;;;; Заметки к проекту

(use-package org-projectile
  :defer t 
  :ensure t
  :functions (org-projectile-per-project)
  :defines (org-projectile-per-project-filepath)
  :after (projectile org)
  :bind (:map projectile-mode-map
                ("C-c pt" . org-projectile-project-todo-completing-read)
                ("C-c c" . org-projectile-capture-for-current-project))
  :config
  
  (setq org-projectile-per-project-filepath "TODO.org")
                                        ;(setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))

  ;; (progn
  ;;   (setq org-projectile-projects-file
  ;;         "/Projects/projects.org")
  ;;   (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;;   (push (org-projectile-project-todo-entry) org-capture-templates))
  :init
  (org-projectile-per-project))

(cl-defun org-projectile-capture-for-current-project
    (&rest additional-options &key capture-template &allow-other-keys)
  "Capture a TODO for the current active project.

If CAPTURE-TEMPLATE is provided use it as the capture template
for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
were part of the capture template definition."
  (declare (obsolete org-project-capture-capture-for-current-project "3.0.1"))
  (interactive)
  (let ((project-name
        (org-project-capture-current-project
         (org-project-capture-strategy-get-backend org-projectile-strategy)))
       (project-file (if (file-exists-p (concat (projectile-project-root) "ЧТОДЕЛ.org"))
                         (concat (projectile-project-root) "ЧТОДЕЛ.org")
                       (concat projectile-project-root "todo.org"))))
    (print project-file)
    (if project-name
        (occ-capture
         (make-instance 'occ-context
                        :category project-name
                        :template (or capture-template
                                      org-project-capture-capture-template)
                        :options (append additional-options `(:file ,project-file))
                        :strategy org-projectile-strategy))
      (error (format "%s is not a recognized project."
                  project-name)))))

;; (cl-defun org-projectile-capture-for-current-project
;;     (&rest additional-options &key capture-template &allow-other-keys)
;;   "Capture a TODO for the current active project.

;; If CAPTURE-TEMPLATE is provided use it as the capture template
;; for the TODO. ADDITIONAL-OPTIONS will be supplied as though they
;; were part of the capture template definition."
;;   (declare (obsolete org-project-capture-capture-for-current-project "3.0.1"))
;;   (interactive)
;;   (let* ((project-root (org-project-capture-current-project-root
;;                        (org-project-capture-strategy-get-backend org-projectile-strategy)))
;;         (project-file (if (file-exists-p (concat project-root "ЧТОДЕЛ.org"))
;;                           (concat project-root "ЧТОДЕЛ.org")
;;                         (concat project-root "todo.org")))
;;         (project-name
;;          (org-project-capture-current-project
;;           (org-project-capture-strategy-get-backend org-projectile-strategy))))
;;     (if project-name
;;         (occ-capture
;;          (make-instance 'occ-context
;;                         :category project-name
;;                         :template (or capture-template
;;                                       org-project-capture-capture-template)
;;                         :options (append additional-options `(:file ,project-file))
;;                         :strategy org-projectile-strategy))
;;       (error (format "%s is not a recognized project."
;;                   project-name)))))

;;;; Текстовые меню - Transient

(require 'установить-из)

;; (use-package transient
;;   :init (установить-из :repo "magit/transient" :name "transient"))

                                        ;(add-to-list 'package--builtin-versions '(transient 0 4 3))

;;;; Система контроля версий Git

(use-package magit
  :defer t 
  :after transient
  :ensure t
  :custom ((magit-log-margin '(t age-abbreviated magit-log-margin-width t 7))
          (magit-after-save-refresh-buffers t))
  :init
  (add-hook 'magit-process-find-password-functions
           'magit-process-password-auth-source))

;; Кто это сделал?

(use-package blamer
  :defer t 
  :ensure t
  :bind (("C-c M-i" . blamer-show-commit-info)
	     ("C-c M-b" . blamer-mode))
  :defer t
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 10)
  :custom-face
  (blamer-face ((t :foreground "#9099AB"
		            :background "#AB9990"
		            :height .9
		            :italic t))))

;; Улучшенная раскраска git diff

;; (use-package magit-delta
;;   :ensure t
;;   :after magit
;;   :commands magit-delta-mode
;;   :hook (magit-mode . magit-delta-mode))

;; Показываем незакоммиченные участки слева

(use-package diff-hl
  :defer t 
  :ensure t
  :functions (global-diff-hl-mode)
  ;;:hook
  ;; (dired-mode . diff-hl-dired-mode-unless-remote)
  :custom
  (diff-hl-side 'right)
  :config
  (global-diff-hl-mode -1))

;;;; Машина времени для GIT

(use-package git-timemachine
  :defer t 
  :ensure t)

;;;; Управление Github и Gitlab

;; (use-package forge
;;   :ensure t
;;   :after magit
;;   :config
;;   (transient-append-suffix 'forge-dispatch '(0)
;; 	["Edit"
;; 	 ("e a" "assignees" forge-edit-topic-assignees)
;; 	 ("e r" "review requests" forge-edit-topic-review-requests)]))

;;;; Автоматизация проекта

(use-package prodigy
  :defer t 
  :ensure t)

;;;; Контейнеры

;; (defun my/auto-vterm-copy-mode-for-docker-logs ()
;;   "Если имя буфера содержит \"docker-container-logs\", активировать vterm-copy-mode."
;;   (when (and (string-match-p "docker-container-logs" (buffer-name))
;;           (derived-mode-p 'vterm-mode))
;;     (vterm-copy-mode)))

(use-package docker
  :defer t 
  :ensure t
  :init
  :config
  ;(add-hook 'vterm-mode-hook #'my/auto-vterm-copy-mode-for-docker-logs)
  )

(use-package dockerfile-mode
  :defer t 
  :ensure t
  :init
  :config)

;; (use-package slime-docker
;; :ensure t
;; :init
;; :config)

(use-package docker-compose-mode
  :defer t 
  :ensure t
  :init
  :config)

;; (use-package docker-tramp
;;   :ensure t
;;   :init
;;   :config)

;;;; Поддержка Gitlab

(use-package gitlab
  :defer t 
  :ensure t)

;; (use-package lab
;;   :init (установить-из :repo "isamert/lab.el"))

;;(use-package github :ensure t)

(provide 'про-управление-проектами)
;;; про-управление-проектами.el ends here
