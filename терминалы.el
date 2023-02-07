;;; терминалы.el --- Терминалы
;;; Commentary:

;; Конфигурация терминалов

;;; Code:
;;;; Мульти-терминалы

(use-package multi-term
  :ensure t
  :defer t
  :bind (
         ("C-c tt" . multi-term)
         ("<s-return>" . multi-term)
         ("C-c tn" . multi-term-next)
         ("C-c tp" . multi-term-prev)
         ("M-±" . multi-term-dedicated-toggle)
         ("C-c to" . multi-term-dedicated-toggle)
         :map term-mode-map
         ("C-c C-j" . переключить-режим-ввода-терминала)
         ("C-c C-k" . переключить-режим-ввода-терминала)
         :map term-raw-map
         ("C-c C-j" . переключить-режим-ввода-терминала)
         ("C-c C-k" . переключить-режим-ввода-терминала))
  :custom ((term-buffer-maximum-size 0)
           (show-trailing-whitespace nil))
  :config
  (add-hook 'term-mode-hook
            (lambda ()
              (add-to-list 'term-bind-key-alist '("C-c C-e" . term-send-esc))
              (add-to-list 'term-bind-key-alist '("C-v" . scroll-up-command))
              (add-to-list 'term-bind-key-alist '("M-v" . scroll-down-command))
              )))

;; Функция для переключение режима перемещения по терминалу

(defun переключить-режим-ввода-терминала ()
  "Toggle term between line mode and char mode."
  (interactive)
  (if (term-in-line-mode)
      (term-char-mode)
    (term-line-mode)))

;;;; Выпадающий терминал Eshell

(use-package shrink-path
  :ensure t
  :demand t)

;;;;; Настройка приглашения Eshell

(defun custom-eshell-prompt ()
  "Настройка приглашения оболочки EShell."
  (let* (
         (git-branch-unparsed
          (shell-command-to-string "git rev-parse --abbrev-ref HEAD 2>/dev/null"))
         (git-branch
          (if (string= git-branch-unparsed "")
              ""
            (substring git-branch-unparsed 0 -1)))
         )
     (format "%s %s%s %s\n%s "
             (all-the-icons-octicon "repo")
             (propertize (car (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
             (propertize (cdr (shrink-path-prompt default-directory)) 'face `(:foreground (face-foreground 'default)))
             (unless (string= git-branch "")
               (propertize (concat "[" git-branch "]") 'face `(:inherit font-lock-string-face)))
             (propertize "$" 'face `(:foreground "#ff79c6"))
             )))

(use-package eshell
  :ensure t
  :custom
  (eshell-prompt-function 'custom-eshell-prompt)
  (eshell-highlight-prompt nil)
  (eshell-hist-ignoredups t)
  (eshell-cmpl-cycle-completions nil)
  (eshell-cmpl-ignore-case t)
  (eshell-ask-to-save-history (quote always))
  (eshell-prompt-regexp "❯❯❯ ")
  (eshell-visual-commands '("htop" "zsh" "vim"))

  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (progn
                (define-key eshell-mode-map "\C-a" 'eshell-bol)
                (define-key eshell-mode-map [up] 'previous-line)
                (define-key eshell-mode-map [down] 'next-line)
                ))))

;; (use-package eshell-did-you-mean
;;   :init
;;   (eshell-did-you-mean-setup)
;;   :ensure t)

(use-package eshell-toggle
  :ensure t
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-use-projectile-root t)
  (eshell-toggle-default-directory "~")
  (eshell-toggle-run-command nil)
  (eshell-toggle-init-function #'eshell-toggle-init-eshell)
  ;; :quelpa
  ;; (eshell-toggle :repo "4DA/eshell-toggle" :fetcher github :version original)
  :bind
  ("M-`" . eshell-toggle)
  ("M-§" . eshell-toggle))

;;;; Другие терминалы

(global-set-key (kbd "C-c tr")
                (lambda () (interactive)
                  (start-process-shell-command "URxvt" nil "urxvt")))

(global-set-key (kbd "C-c tc")
                (lambda () (interactive)
                  (start-process-shell-command "Retro Term" nil "cool-retro-term")))

(use-package vterm
  :ensure t
  :config
  (defun turn-off-chrome ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1))
  :hook (vterm-mode . turn-off-chrome))

(use-package vterm-toggle
  :ensure t
  :custom
  (vterm-toggle-fullscreen-p nil "Open a vterm in another window.")
  (vterm-toggle-scope 'project)
  :bind (("C-c tv" . #'vterm-toggle)
         :map vterm-mode-map
         ("C-\\" . #'popper-cycle)
         ("s-t" . #'vterm)
         ("s-v" . #'vterm-yank)))

(provide 'терминалы)
;;; терминалы.el ends here
