;;; быстрый-доступ.el --- Быстрый поиск имён в вертикальных списках
;;; Commentary:
;;; Code:
;;;; Вертикальные списки
(print "vertico")
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
  :init
  (require 'vertico)
  :config
  (print "vertico loaded")
  (vertico-mode t)
  ;; (setq completion-in-region-function
	;;       (lambda (&rest args)
	;;         (apply (if vertico-mode
	;; 	                 #'consult-completion-in-region
	;; 	               #'completion--in-region)
	;; 	             args)))
  
  )

;;;; Минибуфер во фрейме поверх окна

(use-package mini-frame
  :ensure t
  :custom
  (mini-frame-show-parameters '((child-frame-border-width . 0)
                                (internal-border-width . 0)
                                (top . 0.3)
                                (width . 0.8)
                                (height . 0.3)
                                (left . 0.5)))
  (mini-frame-standalone t)
  (mini-frame-resize nil)
  (mini-frame-color-shift-step 7)
  (mini-frame-internal-border-color "#333333")
  :config   
  (mini-frame-mode t))


;;;; Сортировка

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless basic) completion-category-defaults nil completion-category-overrides
              '((file
                 (styles partial-completion)))))

;;;; Подписи и дополнительная информация

(use-package marginalia
  :ensure t
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config (marginalia-mode))

;;;; Функции на базе автодополения

(use-package consult
  :ensure t
  :custom (
           (consult-preview-key (kbd "M-.")))
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

  ;; (setq register-preview-delay 0.1
	;;   register-preview-function #'consult-register-format)
  ;; (setq xref-show-xrefs-function #'consult-xref
	;;   xref-show-definitions-function #'consult-xref)
  ;; (setq consult-ripgrep-command "rg --null --line-buffered --color=ansi --max-columns=1000 --smart-case --no-heading --line-number . -e ARG OPTS")
  ;; (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --line-number .")
  )

;;;; Дополнение сниппетов

(use-package consult-yasnippet
  :ensure t
  :bind ("C-c y y" . consult-yasnippet))

;;;; Поиск по документации (dash)

(use-package consult-dash
  :ensure t
  :bind (("M-s d" . consult-dash))
  :config (consult-customize consult-dash
                             :initial (thing-at-point 'symbol)))

;;;; Поиск файлов

(defun dobro/consult-ag-from-current-path ()
  "Поиск по файлам от текущего пути."
  (interactive)
  (consult-ag default-directory))

(use-package consult-ag
  :ensure t
  :bind (:map dired-mode-map
              ("s" . dobro/consult-ag-from-current-path)))

;;;; Поиск по языковым серверам

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
