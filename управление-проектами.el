;;; управление-проектами.el --- Управление проектами
;;; Commentary:
;;; Code:
;;;; Модуль для работы с проектами

(leaf projectile
  :ensure t
  :custom ((projectile-sort-order . 'recently-active)
           (projectile-project-search-path . '("~/Проекты/"))
           (projectile-switch-project-action . #'projectile-dired))
  :bind (
         ("s-P" . projectile-add-known-project)
         ("C-c p C-p" . projectile-add-known-project)
         ("C-c pp" . projectile-switch-project)
         ("C-c ps" . consult-ag)
         (:projectile-command-map
         ("ss" . consult-ag))
         )
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (projectile-mode t))

;;;; Заметки к проекту

(leaf org-projectile
  :ensure t
  :after (projectile org)
  :bind ((:projectile-mode-map
         ("C-c pt" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-projectile-capture-for-current-project)))
  :config
    
  (setq org-projectile-per-project-filepath "ЧТОДЕЛ.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  
  ;; (progn
  ;;   (setq org-projectile-projects-file
  ;;         "/Projects/projects.org")
  ;;   (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  ;;   (push (org-projectile-project-todo-entry) org-capture-templates))
  :init
  (org-projectile-per-project))

;;;; Система контроля версий Git

(leaf magit
  :ensure t
  :bind (("C-c g" . magit-status))
  :custom ((magit-log-margin . '(t age-abbreviated magit-log-margin-width t 7))
           (magit-after-save-refresh-buffers . t))
  :init
  (require 'magit-git)
  (require 'magit-process)
  (add-hook 'magit-process-find-password-functions
            'magit-process-password-auth-source))

(leaf magit-todos :ensure t)

;;;; Автоматизация проекта

(leaf prodigy
  :ensure t
  :bind (("C-c C-p" . prodigy)))

;;;; Контейнеры

(leaf docker
  :bind (("C-c d" . docker))
  :ensure t
  :init
  :config)

(leaf dockerfile-mode
  :ensure t
  :init
  :config)

;; (leaf slime-docker
;; :ensure t
;; :init
;; :config)

(leaf docker-compose-mode
  :ensure t
  :init
  :config)

;; (leaf docker-tramp
;;   :ensure t
;;   :init
;;   :config)

;;;; Поддержка Github

;; (leaf github :ensure t)

(provide 'управление-проектами)
;;; управление-проектами.el ends here
