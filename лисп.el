;; * Lisp
;; ** Emacs Lisp

(use-package emacs-lisp
  :straight (:type built-in)
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . eval-buffer)))

;; Функция для реального перевыполнения форм в буфере


(defun my/eval-buffer () 
  "Execute the current buffer as Lisp code.
Top-level forms are evaluated with `eval-defun' so that `defvar'
and `defcustom' forms reset their default values." 
  (interactive) 
  (save-excursion (goto-char (point-min)) 
                  (while (not (eobp)) 
                    (forward-sexp) 
                    (eval-defun nil))))

;; Функция для поиска определения символа в Elisp

                                        ;   (defun find-symbol-at-point ()
                                        ;   "Find the symbol at point, i.e. go to definition."
                                        ;   (interactive)
                                        ;   (let ((sym (symbol-at-point)))
                                        ;     (if (boundp sym)
                                        ; 	(find-variable sym)
                                        ;       (find-function sym))))

                                        ; (define-key lisp-mode-shared-map (kbd "M-.") 'find-symbol-at-point)
                                        ; (define-key emacs-lisp-mode-map (kbd "M-.") 'find-symbol-at-point)


;; Статический анализатор для Elisp

(use-package elsa 
  :ensure t 
  :defer t)

(use-package flycheck-elsa 
  :ensure t 
  :after flycheck 
  :config (flycheck-elsa-setup))

;; Автоматическое тестирование и отладка конфига Emacs

(use-package bug-hunter 
  :disabled t 
  :ensure t 
  :defer t)

;;  Изучение API Elisp

(use-package elisp-demos 
  :ensure t 
  :config)

;; Дополнительная справка

(use-package helpful 
  :ensure t 
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;; **  Geiser

(use-package geiser 
  :ensure t 
  :config)

;;  Geiser Guile
                                        ;
                                        ;(use-package geiser-guile
                                        ;  :config
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/nonguix")
                                        ;  (add-to-list 'geiser-guile-load-path "~/System/channels/chan"))

                                        ;(with-eval-after-load 'yasnippet
                                        ;  (add-to-list 'yas-snippet-dirs "~/System/channels/guix/etc/snippets"))
;; ** Форматирование

;; (use-package elisp-autofmt
;;   :load-path "emacs-lisp/emacs-lisp-autofmt/"
;;   :commands (elisp-autofmt-mode)
;;   :hook (emacs-lisp-mode . elisp-autofmt-mode))

(use-package elisp-format 
  :ensure t
  ;; :hook
  ;; ((emacs-lisp-mode
  ;;         .
  ;;         (lambda
  ;;           ()
  ;;           (add-hook 'before-save-hook #'elisp-format-buffer))))
  )



;; *  dobro-lisp

(provide 'лисп)
