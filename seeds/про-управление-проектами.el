;;; про-управление-проектами.el --- Projectile: проекты -*- lexical-binding: t -*-
;;; Commentary:
;; Управление проектами через Projectile.
;;; Code:

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :custom ((projectile-project-search-path '("~/src/" "~/projects/")))
  :bind-keymap ("C-c p" . projectile-command-map))

(provide 'про-управление-проектами)
;;; про-управление-проектами.el ends here
