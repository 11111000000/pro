;;; программирование-на-lisp.el --- LISP
;;; Commentary:
;;; Code:
;;;; Emacs Lisp

(use-package emacs-lisp
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . перевыполнить-буфер)))

;;;;; Функция для реального перевыполнения форм в буфере

(defun перевыполнить-буфер ()
  "Вызвать выполнения кода в текущем буфере.
Верхне-уровневые формы выполняются `eval-defun', таким
образом `defvar' и `defcustom' устанавливаются заново."
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (not (eobp))
                    (forward-sexp)
                    (eval-defun nil))))

(use-package flymake-elisp-config
  :init (slot/vc-install :repo "ROCKTAKEY/flymake-elisp-config")
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


(provide 'программирование-на-lisp)
;;; программирование-на-lisp.el ends here
