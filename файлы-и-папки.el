;;; файлы-и-папки.el --- Файлы и папки
;;; Commentary:
;; Конфигурация списков файлов и деревьев
;;; Code:
;;;; Файлы и каталоги

(use-package dired
  :bind (("C-x d" . dired-jump)
         ("C-x C-d" . dired-jump)
         :map dired-mode-map
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("l" . dired-find-file)
         ("f" . dired-find-file)
         ("o" . dired-find-file)
         ("h" . dired-up-directory)
         ("b" . dired-up-directory)
         ("u" . dired-up-directory))
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)
         ;;(dired-mode . hl-line-mode)
         )
  :custom 
  ;; (dired-listing-switches "-aBhlv --group-directories-first")
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil)
	(dired-dwim-target t)
  (dired-auto-revert-buffer t)
  (global-auto-revert-non-file-buffers t)
  (dired-hide-details-hide-symlink-targets nil))

;;;; Редактор каталогов WDired

;; Редактирование запускается в Dired с помощью C-c C-c, далее можно произвести
;; действия со списком файлов, как-будто это обычный текст и сохранить изменения,
;; нажав C-c C-c или отменить C-g

(use-package wdired
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
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-indentation                     2
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
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           30
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       nil
          treemacs-workspace-switch-cleanup        nil)

    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("M-t" . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


;;;; Иконки 

(use-package treemacs-icons-dired
  :ensure t
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :init
  (add-hook 'after-load-theme-hook 
          (lambda () 
            (treemacs-icons-dired-mode -1)
            (sleep-for 0 100)
            (treemacs-icons-dired-mode 1))))

;;;; Дерево для проектов

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;;;; Дерево для Git

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;; (use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
;;   :after (treemacs persp-mode) ;;or perspective vs. persp-mode
;;   :ensure t
;;   :config (treemacs-set-scope-type 'Perspectives))

(use-package ag :ensure t)

(provide 'файлы-и-папки)
;;; файлы-и-папки.el ends here
