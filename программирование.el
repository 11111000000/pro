;;; программирование.el --- Конфигурация среды программирования
;;; Commentary:
;;; Code:
;;;; Скобки

;; Подсвечивать все скобки

(use-package paren-face
  :ensure t
  :if window-system
  :custom ((paren-face-regexp
            "[][(){}]"))
  :config (global-paren-face-mode t))

;;;;; Парные скобки

;; Прыгать между парными скобками

(defun forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point.
ARG - backward"
  (interactive "^p")
  (cond
   ((looking-at "\\s(") (forward-sexp arg))
   ((looking-back "\\s)" 1) (backward-sexp arg))
   ((looking-at "\\s)") (forward-char) (backward-sexp arg))
   ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;;;; Умные скобки

;; TODO: заменить пакетом, основанным на tree-sitter ?

(use-package smartparens
  :ensure t
  :bind  (("C-^" . sp-unwrap-sexp)
          ("M-j" . sp-next-sexp)
          ("M-k" . sp-backward-sexp)
          ("M-h" . sp-backward-up-sexp)
          ("M-l" . sp-down-sexp))
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

;;;;; Подсветка глубины скобок

(defvar my/hl-paren-face)
(setq my/hl-paren-face (face-foreground 'default))

(use-package highlight-parentheses
  :ensure t
  :custom ((hl-paren-colors
            `("MediumOrchid2" "MediumAquamarine" "CornflowerBlue" ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face
              ,my/hl-paren-face ,my/hl-paren-face ,my/hl-paren-face))
           (hl-paren-background-colors  '(nil nil nil nil nil)))
  :config (global-highlight-parentheses-mode t)
  :init
  (add-hook 'after-load-theme-hook
            (lambda ()
              (global-highlight-parentheses-mode -1)
              (sleep-for 0 100)
              (global-highlight-parentheses-mode t)
              )))

;;;; Идентификаторы

(use-package color-identifiers-mode
  :if  window-system
  :ensure t
  :bind (("C-c hi" . color-identifiers-mode))
  :hook ((js-mode . color-identifiers-mode)
         ;;(typescript-mode . color-identifiers-mode)
         )
  :custom ((color-identifiers-coloring-method
            'hash)
           (color-identifiers:num-colors 16)
           (color-identifiers:color-luminance 0.4)
           (color-identifiers:min-color-saturation 0.2)
           (color-identifiers:max-color-saturation 0.7)))

(use-package rainbow-identifiers
  :if window-system
  :ensure t
  :defer t
  :hook ((typescript-mode . rainbow-identifiers-mode)
         (emacs-lisp-mode . rainbow-identifiers-mode))
  :bind (("C-c hl" . rainbow-identifiers-mode)))

;;;; Форматирование

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


;;;; Подсветка цветов

(use-package rainbow-mode
  :hook (prog-mode)
  :ensure t)

;;;; Статическая проверка кода

(use-package flymake
  :hook ((prog-mode) . flymake-mode)

  :bind (:map flymake-mode-map
              ("C-c ]" . flymake-goto-next-error)
              ("C-c [" . flymake-goto-prev-error)))

;;;; Сообщения статического анализатора во всплывающем окне

;; (use-package emacs-flymake-popon
;;   :init
;;   (unless (package-installed-p 'emacs-flymake-popon)
;;    (package-vc-install "https://codeberg.org/akib/emacs-flymake-popon.git"))
;;   :custom ((flymake-popon-delay .8)
;;            (flymake-popon-posframe-extra-arguments '(:poshandler posframe-poshandler-point-bottom-left-corner)))
;;   :hook ((flymake-mode) . flymake-popon-mode))

;; (use-package flymake-posframe
;;   :load-path "emacs-lisp/flymake-posframe"
;;   :hook (flymake-mode . flymake-posframe-mode))

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

;;;; Сниппеты

(defvar dobro/yas-new-snippet-prompt-file (concat (file-name-directory (locate-library "программирование")) "etc/yasnippet.template.txt"))

(defun dobro/yas-new-snippet-with-example ()
  "Создать новый сниппет со шпаргалкой."
  (interactive)
  (funcall-interactively 'yas-new-snippet)
  (erase-buffer)
  (insert-file dobro/yas-new-snippet-prompt-file))

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :bind (("C-c y v" . yas-visit-snippet-file)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y y" . yas-insert-snippet)
         ("C-c y n" . dobro/yas-new-snippet-with-example))
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :init)

;;;; Дерево синтаксиса

;; Генератор инкрементальных парсеров

;; (use-package tree-sitter
;;   :config (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

;;;; Языковой сервер

;; Eglot теперь встроен в EMACS

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
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

;;;; Дебаггер

;; (use-package dap-mode
;;   :ensure t
;;   :bind (:map dap-mode-map
;;               ("C-c bb" . dap-ui-breakpoints)
;;               ("C-c bl" . dap-ui-locals)
;;               ("C-c bt" . dap-breakpoint-toggle)
;;               ("C-c bd" . dap-breakpoint-delete)
;;               ("C-c bi" . dap-step-in)
;;               ("C-c bo" . dap-step-out)
;;               ("C-c bc" . dap-continue)
;;               ("<f5>" . dap-continue)
;;               ("<f6>" . dap-step-in)
;;               ("<f7>" . dap-step-out))
;;   :config
;;   (setq dap-auto-configure-features '(locals expression breakpoints))
;;   (загрузить 'dap-chrome)
;;   (загрузить 'dap-firefox)
;;   (загрузить 'dap-node))

;;;; Конструктор регулярных выражений

;; Замечательный инструмент, позволяющий визуально контролировать создание поисковых выражений

(загрузить 're-builder)

;; Используем синтаксис string (так меньше экранировать спец. символы)

(setq reb-re-syntax 'read)


(provide 'программирование)
;;; программирование.el ends here
