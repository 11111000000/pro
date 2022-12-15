;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Вертикальные списки

(use-package vertico 
  :ensure t 
  :custom
  (read-file-name-completion-ignore-case t) 
  (read-buffer-completion-ignore-case t) 
  (vertico-cycle t)
  (completion-ignore-case t)
  :bind (:map vertico-map
         ("s-<tab>" . vertico-next)
         ("s-<iso-lefttab>" . vertico-previous))
  :config
  ;; (setq completion-in-region-function
	;;       (lambda (&rest args)
	;;         (apply (if vertico-mode
	;; 	                 #'consult-completion-in-region
	;; 	               #'completion--in-region)
	;; 	             args)))
  :init (vertico-mode t))

;;; Сортировка

(use-package orderless 
  :ensure t
  :init (setq completion-styles '(orderless basic) completion-category-defaults nil completion-category-overrides 
              '((file 
                 (styles partial-completion)))))

;;; Подписи и дополнительная информация

(use-package marginalia 
  :ensure t 
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil)) 
  :init (marginalia-mode))

;;; Функции на базе автодополения

(use-package consult 
  :ensure t 
  :bind (("C-x b" . consult-buffer) 
         ("C-x C-b" . consult-buffer-other-window)
         ("s-b" . consult-buffer)
         ("s-<tab>" . consult-buffer)
         ("M-s s" . consult-line)
         ("M-s M-s" . consult-line-multi)
         ("C-x y" . consult-yank-from-kill-ring)
         ("<help> a" . consult-apropos)
         ("s-m" . consult-imenu-multi)
         ) 
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :init

  ;; (setq register-preview-delay 0.1
	;;   register-preview-function #'consult-register-format)
  ;; (setq xref-show-xrefs-function #'consult-xref
	;;   xref-show-definitions-function #'consult-xref)
  ;; (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --smart-case --no-heading --line-number . -e ARG OPTS")
  ;; (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
  )

;;; Дополнение сниппетов

(use-package consult-yasnippet 
  :ensure t 
  :bind ("C-c y y" . consult-yasnippet))

;;; Поиск по документации (dash)

(use-package consult-dash 
  :ensure t 
  :bind (("M-s d" . consult-dash)) 
  :config (consult-customize consult-dash 
                             :initial (thing-at-point 'symbol)))

;;; Поиск файлов

(defun dobro/consult-ag-from-current-path () 
  (interactive)
  (consult-ag default-directory))

(use-package consult-ag 
  :ensure t 
  :bind (:map dired-mode-map
              ("s" . dobro/consult-ag-from-current-path)))

;;; Поиск по языковым серверам

(use-package consult-lsp
  :after (lsp consult)
  :ensure t
  :disabled t)

(use-package consult-eglot
  :after (eglot)
  :ensure t
  :bind (:map eglot-mode-map ("s-t" . #'consult-eglot-symbols)))

(provide 'быстрый-доступ)
;;; быстрый-доступ.el ends here
