;;; про-файлы-и-папки.el --- Файлы и папки -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация списков файлов и деревьев
;;; Code:

(require 'use-package)

;;;; Файлы и каталоги

(defun директорию-вверх () "Перейти на директорию вверх."
      (interactive) (find-file ".."))

(use-package dired
  :bind (
         :map dired-mode-map
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("l" . dired-find-file)
         ("f" . dired-find-file)
         ("o" . dired-find-file)
         ("RET" . dired-find-file)
         ("h" . dired-up-directory)
         ("^" . dired-up-directory))
  :hook ((dired-mode . dired-hide-details-mode)
       (dired-mode . hl-line-mode)
       ;;(dired-mode . hl-line-mode)
       )
  :custom
  (dired-listing-switches "-aBhlv --group-directories-first")
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (global-auto-revert-non-file-buffers t)
  (dired-hide-details-hide-symlink-targets nil)
  ;;dired открывает в том же окне при использовании RET или ^
  ;; (put 'dired-find-alternate-file 'disabled nil)

  )

(use-package async-await
  :defer t 
  :ensure t
  :functions (dired-async-mode)
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

;;;; Функция выполнить команду в выбранных файлах

(require 'dired)

(defun dired-do-command (command)
  "Выполняет команду COMMAND для помеченных файлов.
Все файлы, которые еще не открыты, будут открыты.
После того, как эта команда будет выполнена, все
буферы, которые она изменила, останутся открытые и неспасенные."
  (interactive " M-x на выбранных файлах: ")
  (save-window-excursion
    (mapc (lambda (filename)
             (find-file filename)
             (call-interactively command))
           (dired-get-marked-files))))

;;;; Редактор каталогов

;; Редактирование запускается в Dired с помощью C-c C-c, далее можно произвести
;; действия со списком файлов, как-будто это обычный текст и сохранить изменения,
;; нажав C-c C-c или отменить C-g

(use-package wdired
  :defer t 
  :ensure t
  :after dired
  :bind (
         :map dired-mode-map
         ("C-c C-c" . wdired-change-to-wdired-mode)
         :map wdired-mode-map
         ("C-c C-r" . replace-string)
         ("C-c r" . replace-regexp)
         ("C-g C-g" . wdired-exit)
         ("ESC" . wdired-exit)))

;;;; Дерево

(use-package treemacs
  :ensure t
  :defer t
  :functions (treemacs-follow-mode treemacs-filewatch-mode treemacs-select-window
                              treemacs-fringe-indicator-mode treemacs-git-mode
                              treemacs-hide-gitignored-files-mode treemacs-icons-dired-mode)
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind (:map treemacs-mode-map
                ("j" . treemacs-next-line)
                ("k" . treemacs-previous-line))
  :config
  (progn
    (setq treemacs-collapse-dirs             (if treemacs-python-executable 3 0)
         treemacs-deferred-git-apply-delay        0.5
         treemacs-directory-name-transformer      #'identity
         treemacs-display-in-side-window          t
         treemacs-eldoc-display                   'simple
         treemacs-file-event-delay                3000
         treemacs-file-extension-regex            treemacs-last-period-regex-value
         treemacs-file-follow-delay               0.1
         treemacs-file-name-transformer           #'identity
         treemacs-follow-after-init               t
         treemacs-expand-after-init               t
         treemacs-find-workspace-method           'find-for-file-or-pick-first
         treemacs-git-command-pipe                ""
         treemacs-goto-tag-strategy               'refetch-index
         treemacs-indentation                     1
         treemacs-indentation-string              " "
         treemacs-is-never-other-window           nil
         treemacs-max-git-entries                 5000
         treemacs-missing-project-action          'remove
         treemacs-move-forward-on-expand          nil
         treemacs-no-png-images                   nil
         treemacs-no-delete-other-windows         t
         treemacs-project-follow-cleanup          nil
         treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
         treemacs-position                        'left
         treemacs-read-string-input               'from-child-frame
         treemacs-recenter-distance               0.1
         treemacs-recenter-after-file-follow      nil
         treemacs-recenter-after-tag-follow       nil
         treemacs-recenter-after-project-jump     'always
         treemacs-recenter-after-project-expand   'on-distance
         treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
         treemacs-show-cursor                     nil
         treemacs-show-hidden-files               t
         treemacs-silent-filewatch                nil
         treemacs-silent-refresh                  nil
         treemacs-sorting                         'alphabetic-asc
         treemacs-select-when-already-in-treemacs 'move-back
         treemacs-space-between-root-nodes        nil
         treemacs-tag-follow-cleanup              t
         treemacs-tag-follow-delay                1
         treemacs-text-scale                      0.5
         treemacs-user-mode-line-format           nil
         treemacs-user-header-line-format         nil
         treemacs-wide-toggle-width               80
         treemacs-width                           21
         treemacs-width-increment                 1
         treemacs-width-is-initially-locked       nil
         treemacs-workspace-switch-cleanup        nil)

    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'only-when-focused)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil)))


;;;; Иконки


;;;; Дерево для проектов

(use-package treemacs-projectile
  :ensure t
  :defer t
  :after (treemacs projectile))

;;;; Дерево для Git

(use-package treemacs-magit
  :ensure t
  :defer t
  :after (treemacs magit))

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package ag
  :defer t  :ensure t)

(use-package dired-toggle-sudo
  :ensure t
  :defer t)

(provide 'про-файлы-и-папки)
;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:
;;; про-файлы-и-папки.el ends here
