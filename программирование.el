;; * Код
;; ** Дерево синтаксиса

;; Генератор инкрементальных парсеров

(use-package tree-sitter 
  :ensure t 
  :config (global-tree-sitter-mode) 
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;; ** Языковой сервер

;; Eglot теперь встроен в EMACS

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (rust-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-c a r" . #'eglot-rename)
              ("C-<down-mouse-1>" . #'xref-find-definitions)
              ("C-S-<down-mouse-1>" . #'xref-find-references)
              ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (put 'typescript-react-mode 'eglot-language-id "typescriptreact")
  (add-to-list 'eglot-server-programs `(typescript-react-mode . ("typescript-language-server" "--stdio"))))

;; ** Скобки

;; Подсвечивать все скобки

(use-package paren-face 
  :ensure t 
  :if window-system 
  :custom ((paren-face-regexp 
            "[][(){}]")) 
  :config (global-paren-face-mode t))

;; *** Парные скобки

;; Прыгать между парными скобками

(defun forward-or-backward-sexp 
    (&optional 
     arg)
  "Go to the matching parenthesis character if one is adjacent to point.
ARG - backward" 
  (interactive "^p") 
  (cond 
   ((looking-at "\\s(") (forward-sexp arg)) 
   ((looking-back "\\s)" 1) (backward-sexp arg)) 
   ((looking-at "\\s)") (forward-char) (backward-sexp arg)) 
   ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

(global-set-key (kbd "C-%") 'forward-or-backward-sexp)

;; *** Умные скобки

;; TODO: заменить пакетом, основанным на tree-sitter ?

(use-package smartparens 
  :ensure t 
  :diminish "()" 
  :bind  (("C-^" . sp-unwrap-sexp) 
          ("M-j" . sp-next-sexp) 
          ("M-k" . sp-backward-sexp) 
          ("M-h" . sp-backward-up-sexp) 
          ("M-l" . sp-down-sexp)
          ;; :map org-mode-map
          ;; ("M-j" . sp-next-sexp)
          ;; ("M-k" . sp-backward-sexp)
          ;; ("M-h" . sp-backward-up-sexp)
          ;; ("M-l" . sp-down-sexp)
          ) 
  :init (smartparens-global-mode 1) 
  (sp-local-pair 'emacs-lisp-mode "'" nil 
                 :actions nil) 
  (sp-local-pair 'scheme-mode "'" nil 
                 :actions nil) 
  (sp-local-pair 'racket-mode "'" nil 
                 :actions nil) 
  (sp-local-pair 'lisp-mode "" nil 
                 :actions nil) 
  (show-smartparens-global-mode t)
  ;; (show-paren-mode -1)
  )

;; *** Подсветка глубины скобок

(defvar my/hl-paren-face)
(setq my/hl-paren-face (face-foreground 'default))

(use-package highlight-parentheses 
  :ensure t 
  :custom ((hl-paren-colors 
            `("MediumOrchid2" "MediumAquamarine" "CornflowerBlue" ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face
              ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face)) 
           (hl-paren-background-colors  '(nil nil nil nil nil))) 
  :config (global-highlight-parentheses-mode t))

;; ** Идентификаторы

(use-package color-identifiers-mode 
  :ensure t 
  :diminish (color-identifiers-mode . "≡ ")
  
  :bind (("C-c hi" . color-identifiers-mode))
  :hook (
         (js-mode . color-identifiers-mode) 
        ;;(typescript-mode . color-identifiers-mode)
         )
  :custom ((color-identifiers-coloring-method 
            'hash) 
           (color-identifiers:num-colors 16) 
           (color-identifiers:color-luminance 0.4) 
           (color-identifiers:min-color-saturation 0.2) 
           (color-identifiers:max-color-saturation 0.7)))

(use-package rainbow-identifiers 
  :ensure t 
  :defer t 
  :diminish (rainbow-identifiers-mode . "≡ ") 
  :hook ((typescript-mode . rainbow-identifiers-mode)
         (emacs-lisp-mode . rainbow-identifiers-mode))
  :bind (("C-c hl" . rainbow-identifiers-mode)))

;; ** Форматирование

(use-package format-all 
  :ensure t 
  :hook ((ess-r-mode . format-all-mode) 
         (python-mode . format-all-mode)
         ;; (emacs-lisp-mode . format-all-mode)
         (format-all-mode-hook . format-all-ensure-formatter)) 
  :config (custom-set-variables '(format-all-formatters (quote (("Python" black) 
                                                                ("R" styler))))))

;; (use-package apheleia
;;   :ensure t
;;   :hook ((js-mode . aphelia-mode)
;;          (typescript-mode . aphelia-mode))
;;   :config
;;   ;;(apheleia-global-mode nil)
;;   )

;; ** Сниппеты

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :bind
  (("C-c y n" . yas-new-snippet)
   ("C-c y v" . yas-visit-snippet-file)
   ("C-c y i" . yas-insert-snippet)
   ("C-c y y" . yas-insert-snippet)
   )
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

;; (use-package yasnippet 
;;   :ensure t 
;;   :init (yas-global-mode 1))

(use-package yasnippet-snippets 
  :ensure t 
  :init)

;; ** Подсветка цветов

(use-package rainbow-mode
  :hook (prog-mode)
  :ensure t)

;; ** Flymake

(use-package flymake
  :hook ((prog-mode) . flymake-mode)
  :bind (:map flymake-mode-map
               ("M-n" . flymake-goto-next-error)
               ("M-p" . flymake-goto-prev-error))
  )

;; ** Ошибки Flymake во всплывающем окне

;; (use-package flymake-posframe  
;;   :load-path "emacs-lisp/flymake-posframe"
;;   :hook (flymake-mode . flymake-posframe-mode))

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))


(provide 'программирование)

;;; dobro-code.el ends here
