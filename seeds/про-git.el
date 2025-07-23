;;; про-git.el --- Минималистичная поддержка git -*- lexical-binding: t -*-
;;; Commentary:
;; Magit, diff-hl для индикации изменений, работает если найден git.
;;; Code:

(when (executable-find "git")
  (use-package magit
    :ensure t
    :commands (magit-status))
  (use-package diff-hl
    :ensure t
    :hook ((prog-mode . diff-hl-mode)
           (magit-post-refresh . diff-hl-magit-post-refresh)))
  (message "Git seed активирован."))

(provide 'про-git)
;;; про-git.el ends here
