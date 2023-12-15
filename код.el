;;; код.el --- Конфигурация среды программирования
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
              (global-highlight-parentheses-mode t))))


;;;; Идентификаторы

;; Удобно подсветить разные идентификаторы разными цветами

(use-package color-identifiers-mode
  :if  window-system
  :ensure t
  :hook ((js-mode . color-identifiers-mode))
  :custom ((color-identifiers-coloring-method
            'hash)
           (color-identifiers:num-colors 16)
           (color-identifiers:color-luminance 0.4)
           (color-identifiers:min-color-saturation 0.2)
           (color-identifiers:max-color-saturation 0.7)))

;; Альтернативный алгоритм подсветки идентификаторов

(use-package rainbow-identifiers
  :if window-system
  :ensure t
  :defer t
  :hook (
       (typescript-ts-mode . rainbow-identifiers-mode)
         (emacs-lisp-mode . rainbow-identifiers-mode)))

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
  :ensure t
  :hook (prog-mode))

;;;; Статическая проверка кода

(use-package flymake
  :ensure t
  :custom ((flymake-gui-warnings-enabled . nil))
  :hook ((prog-mode) . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-]" . flymake-goto-next-error)
              ("M-[" . flymake-goto-prev-error)))

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

;; Быстрые шаблоны - сниппеты можно создавать на лету со шпаргалкой

(defvar шаблон-для-сниппета (concat
                             (file-name-directory
                              (locate-library "код"))
                             "etc/yasnippet.template.txt"))

(defun создать-новый-сниппет-со-шпаргалкой ()
  "Создать новый сниппет со шпаргалкой."
  (interactive)
  (funcall-interactively 'yas-new-snippet)
  (erase-buffer)
  (insert-file шаблон-для-сниппета))

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
        '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :init)

;;;; Дерево синтаксиса

;; Генератор инкрементальных парсеров
(use-package treesit
  :when (and (fboundp 'treesit-available-p)
             (treesit-available-p))
  :custom (major-mode-remap-alist
           '((c-mode          . c-ts-mode)
             (c++-mode        . c++-ts-mode)
             (cmake-mode      . cmake-ts-mode)
             (conf-toml-mode  . toml-ts-mode)
             (css-mode        . css-ts-mode)
             (js-mode         . js-ts-mode)
             (js-json-mode    . json-ts-mode)
             (python-mode     . python-ts-mode)
             (sh-mode         . bash-ts-mode)
             (typescript-mode . typescript-ts-mode)))
  :config
  ;; (setq treesit-language-source-alist
  ;;     '((bash "https://github.com/tree-sitter/tree-sitter-bash")
  ;;       (cmake "https://github.com/uyha/tree-sitter-cmake")
  ;;       (css "https://github.com/tree-sitter/tree-sitter-css")
  ;;       (elisp "https://github.com/Wilfred/tree-sitter-elisp")
  ;;       (go "https://github.com/tree-sitter/tree-sitter-go")
  ;;       (html "https://github.com/tree-sitter/tree-sitter-html")
  ;;       (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
  ;;       (json "https://github.com/tree-sitter/tree-sitter-json")
  ;;       (make "https://github.com/alemuller/tree-sitter-make")
  ;;       (markdown "https://github.com/ikatyang/tree-sitter-markdown")
  ;;       (python "https://github.com/tree-sitter/tree-sitter-python")
  ;;       (toml "https://github.com/tree-sitter/tree-sitter-toml")
  ;;       (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
  ;;       (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
  ;;       (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  )

;; (use-package treesit-auto
;;   :after (treesit)
;;   :ensure t
;;   :config
;;   (require 'treesit)
;;   (global-treesit-auto-mode))

;; (use-package tree-sitter
;;   :config (global-tree-sitter-mode)
;;   (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;;;; Языковой сервер

;; Eglot теперь встроен в EMACS

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
       (haskell-mode . eglot-ensure)
       (typescript-mode . eglot-ensure)
       (rust-mode . eglot-ensure)
       (typescript-ts-mode . eglot-ensure)
       (haskell-mode . eglot-ensure)
       (js-mode . eglot-ensure)
       (json-mode . eglot-ensure)
       (rust-mode . eglot-ensure))
  :bind (:map eglot-mode-map
                ("C-c r" . #'eglot-rename)
                ("C-<down-mouse-1>" . #'xref-find-definitions)
                ("C-S-<down-mouse-1>" . #'xref-find-references)
                ("C-c C-c" . #'eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
   ;; TODO вытащить в переменную путь к серверу
  )

;; (use-package sideline
;;   :ensure t
;;   :hook ((flycheck-mode . sideline-mode)
;;        (flymake-mode  . sideline-mode)
;;        (eldoc . sideline-mode))
;;   :init
;;   ;; (setq sideline-backends-left '(sideline-flymake sideline-blame)
;;   ;;       sideline-backends-right '(sideline-eldoc))
;;   (setq sideline-backends-left-skip-current-line nil   ; don't display on current line (left)
;;         sideline-backends-right-skip-current-line nil  ; don't display on current line (right)
;;         sideline-order-left 'down                    ; or 'up
;;         sideline-order-right 'up                     ; or 'down
;;         sideline-format-left "%s   "                 ; format for left aligment
;;         sideline-format-right "   %s"                ; format for right aligment
;;         sideline-priority 100                        ; overlays' priority
;;         sideline-display-backend-name t))
                                        ; display the backend name

;; (use-package sideline-flymake :ensure t :init (require 'sideline-flymake))
;; (use-package sideline-blame :ensure t)
;; (use-package sideline-eldoc
;; :init
;; (установить-из :repo "ginqi7/sideline-eldoc")
;; (require 'sideline-eldoc))

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


(provide 'код)
;;; код.el ends here
