;;; быстрый-доступ.el --- Быстрый поиск имён в вертикальных списках
;;; Commentary:
;;; Code:
;;;; Вертикальные списки

(leaf vertico
  :ensure t
  :custom ((read-file-name-completion-ignore-case . t)
           (read-buffer-completion-ignore-case . t)
           (vertico-cycle . t)
           (completion-ignore-case . t))
  :bind ((:vertico-map
          ("s-<tab>" . vertico-next)
          ("s-<iso-lefttab>" . vertico-previous)))
  :config
  ;; (setq completion-in-region-function
	;;       (lambda (&rest args)
	;;         (apply (if vertico-mode
	;; 	                 #'consult-completion-in-region
	;; 	               #'completion--in-region)
	;; 	             args)))
  :init (vertico-mode t))

;;;; Сортировка

(leaf orderless
  :ensure t
  :init (setq completion-styles '(orderless basic) completion-category-defaults nil completion-category-overrides
              '((file
                 (styles partial-completion)))))

;;;; Подписи и дополнительная информация

(leaf marginalia
  :ensure t
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

;;;; Функции на базе автодополения

(leaf consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer-other-window)
         ("s-b" . consult-buffer)
         ("s-<tab>" . consult-buffer)
         ("M-s s" . consult-line)
         ("M-s M-s" . consult-line-multi)
         ("C-x y" . consult-yank-from-kill-ring)
         ("<help> a" . consult-apropos)
         ("s-m" . consult-imenu-multi))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref
	      consult-preview-key (kbd "M-."))
  :init

  ;; (setq register-preview-delay 0.1
	;;   register-preview-function #'consult-register-format)
  ;; (setq xref-show-xrefs-function #'consult-xref
	;;   xref-show-definitions-function #'consult-xref)
  ;; (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --smart-case --no-heading --line-number . -e ARG OPTS")
  ;; (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
  )

;;;; Дополнение сниппетов

(leaf consult-yasnippet
  :ensure t
  :bind ("C-c y y" . consult-yasnippet))

;;;; Поиск по документации (dash)

(leaf consult-dash
  :ensure t
  :bind (("M-s d" . consult-dash))
  :config (consult-customize consult-dash
                             :initial (thing-at-point 'symbol)))

;;;; Поиск файлов


(defun поиск-текста-рекурсивно-от-текущего-пути ()
    "Поиск по файлам от текущего пути."
    (interactive)
    (consult-ag default-directory))

(leaf consult-ag
  :ensure t  
  :bind ((:dired-mode-map
          ("s" . поиск-текста-рекурсивно-от-текущего-пути)))
  :init
  (require 'consult-ag)
  )

;;;; Поиск по языковым серверам

(leaf consult-lsp
  :after (lsp consult)
  :ensure t
  :disabled t)

(leaf consult-eglot
  :after (eglot)
  :ensure t
  :bind ((:eglot-mode-map ("s-t" . consult-eglot-symbols))))

(provide 'быстрый-доступ)
;;; быстрый-доступ.el ends here
