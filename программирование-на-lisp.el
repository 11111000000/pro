;;; программирование-на-lisp.el --- LISP
;;; Commentary:
;;; Code:
;;;; Emacs Lisp

;; Функция для реального перевыполнения форм в буфере

(defun перевыполнить-буфер ()
  "Вызвать выполнения кода в текущем буфере.
Верхне-уровневые формы выполняются `eval-defun', таким
образом `defvar' и `defcustom' устанавливаются заново."
  (interactive)
  (save-excursion (goto-char (point-min))
                  (while (not (eobp))
                    (forward-sexp)
                    (eval-defun nil))))
;; (leaf emacs-lisp
;;   :bind ((:emacs-lisp-mode-map
;;           ("C-c C-c" . перевыполнить-буфер))))

(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'перевыполнить-буфер)

(leaf flymake-elisp-config
  :el-get ROCKTAKEY/flymake-elisp-config
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))


;;;;; Статический анализатор для Elisp

(leaf elsa :ensure t)

(leaf flycheck-elsa
  :ensure t
  :after flycheck
  :config (flycheck-elsa-setup))

;;;;; Автоматическое тестирование и отладка конфига Emacs

(leaf bug-hunter
  :disabled t
  :ensure t)


;;;;; Форматирование ELISP

(leaf elisp-format
  :ensure t
  ;; :hook
  ;; ((emacs-lisp-mode
  ;;         .
  ;;         (lambda
  ;;           ()
  ;;           (add-hook 'before-save-hook #'elisp-format-buffer))))
  )

;; (leaf elisp-autofmt
;;   :load-path "emacs-lisp/emacs-lisp-autofmt/"
;;   :commands (elisp-autofmt-mode)
;;   :hook (emacs-lisp-mode . elisp-autofmt-mode))


;;;; REPL к разным LISP-ам

(leaf geiser
  :custom
  ((geiser-default-implementation . 'guile)
  (geiser-active-implementations . '(guile))
  (geiser-implementations-alist . '(((regexp "\\.scm$") guile))))
  :config
  (setq geiser-mode-start-repl-p nil))

(leaf geiser-guile
  :config
  ;;(add-to-list 'geiser-guile-load-path "~/Workspace/guix")
  (setq geiser-guile-manual-lookup-nodes
	      '("guile"
          "guix")))

;;  Geiser Guile
                                        ;
                                        ;(leaf geiser-guile
                                        ;  :config
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/nonguix")
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/chan"))

                                        ;(with-eval-after-load 'yasnippet
                                        ;  (add-to-list 'yas-snippet-dirs "~/System/channels/guix/etc/snippets"))


(provide 'программирование-на-lisp)
;;; программирование-на-lisp.el ends here
