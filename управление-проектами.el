;;; управление-проектами.el --- Управление проектами
;;; Commentary:
;;; Code:
;;;; Модуль для работы с проектами

(use-package projectile
  :ensure t
  :defines (projectile-command-map)
  :custom ((projectile-sort-order 'recently-active)
                                        ;(projectile-project-search-path '("~/Проекты/"))
          (projectile-switch-project-action #'projectile-dired))
  :bind (:map projectile-command-map
                ("ss" . consult-ag))
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode t))

;;;; Заметки к проекту

(use-package org-projectile
  :ensure t
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

;;;; Система контроля версий Git

(use-package magit
  :ensure t
  :custom ((magit-log-margin '(t age-abbreviated magit-log-margin-width t 7))
          (magit-after-save-refresh-buffers t))
  :init
  (add-hook 'magit-process-find-password-functions
           'magit-process-password-auth-source))

;; Улучшенная раскраска git diff

(use-package magit-delta
  :ensure t
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode))

;; Показываем незакоммиченные участки слева

(use-package diff-hl
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
  :ensure t)

;;;; Контейнеры

(use-package docker
  :ensure t
  :init
  :config)

(use-package dockerfile-mode
  :ensure t
  :init
  :config)

;; (use-package slime-docker
;; :ensure t
;; :init
;; :config)

(use-package docker-compose-mode
  :ensure t
  :init
  :config)

;; (use-package docker-tramp
;;   :ensure t
;;   :init
;;   :config)

;;;; Поддержка Gitlab

(use-package gitlab
  :ensure t)

;; (use-package lab
;;   :init (установить-из :repo "isamert/lab.el"))

;;(use-package github :ensure t)

(provide 'управление-проектами)
;;; управление-проектами.el ends here
