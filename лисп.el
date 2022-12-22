;;; лисп.el --- LISP
;;; Commentary:
;;; Code:
;;;; Emacs Lisp

(use-package emacs-lisp
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . my/eval-buffer)))

;;;;; Функция для реального перевыполнения форм в буфере

(defun my/eval-buffer ()
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values."
  (interactive)
  (save-excursion (goto-char (point-min)) 
                  (while (not (eobp)) 
                    (forward-sexp) 
                    (eval-defun nil))))

(use-package flymake-elisp-config
  :straight '(flymake-elisp-config :repo "https://github.com/ROCKTAKEY/flymake-elisp-config.git")
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))


;;;;; Статический анализатор для Elisp

(use-package elsa 
  :ensure t 
  :defer t)

(use-package flycheck-elsa 
  :ensure t 
  :after flycheck 
  :config (flycheck-elsa-setup))

;;;;; Автоматическое тестирование и отладка конфига Emacs

(use-package bug-hunter 
  :disabled t 
  :ensure t 
  :defer t)


;;;;; Форматирование ELISP

(use-package elisp-format 
  :ensure t
  ;; :hook
  ;; ((emacs-lisp-mode
  ;;         .
  ;;         (lambda
  ;;           ()
  ;;           (add-hook 'before-save-hook #'elisp-format-buffer))))
  )

;; (use-package elisp-autofmt
;;   :load-path "emacs-lisp/emacs-lisp-autofmt/"
;;   :commands (elisp-autofmt-mode)
;;   :hook (emacs-lisp-mode . elisp-autofmt-mode))


;;;; REPL к разным LISP-ам

(use-package geiser
  :ensure t
  :custom 
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  :config
  (setq geiser-mode-start-repl-p nil))

(use-package geiser-guile
  :requires geiser
  :config
  ;;(add-to-list 'geiser-guile-load-path "~/Workspace/guix")
  (setq geiser-guile-manual-lookup-nodes
	      '("guile"
          "guix")))

;;  Geiser Guile
                                        ;
                                        ;(use-package geiser-guile
                                        ;  :config
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/nonguix")
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/chan"))

                                        ;(with-eval-after-load 'yasnippet
                                        ;  (add-to-list 'yas-snippet-dirs "~/System/channels/guix/etc/snippets"))


(provide 'лисп)
;;; лисп.el ends here
