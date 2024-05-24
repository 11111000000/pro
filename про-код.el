;;; про-код.el --- Конфигурация среды программирования
;;; Commentary:
;;; Code:

(require 'загрузить)
(require 'use-package)

;;;; Синтаксис

;; Генератор инкрементальных парсеров tree sitter

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
  :config)

;;;; Cинтаксис для открытого файла

(use-package treesit-auto
  :ensure t
  :functions (treesit-auto-add-to-auto-mode-alist global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt) ; Спрашивать при установке
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;;;; Языковой сервер

;; Языковой сервер Eglot теперь встроен в EMACS

(require 'jsonrpc)

(use-package eglot
  :functions (eglot-rename eglot-code-actions)
  :bind (:map eglot-mode-map
                ("C-c r" . eglot-rename)
                ("M-'" . eglot-inlay-hints-mode)
                ("C-<down-mouse-1>" . xref-find-definitions)
                ("M-." . xref-find-definitions)
                ("C-M-." . xref-find-references)
                ("C-S-<down-mouse-1>" . xref-find-references)
                ("C-c ." . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 3)
  (eglot-events-buffer-size '(:size 2000 :format full))
  (eglot-send-changes-idle-time 0)
  :config
  
  ;; Выключим лог, что увеличивает производительность
  
  (fset #'jsonrpc--log-event #'ignore)
  )


;;;; Подсказки поверх кода

(use-package coverlay :ensure t)

;;;; Скобки

;; Подсвечивать все скобки

(use-package paren-face
  :ensure t
  :if window-system
  :functions (global-paren-face-mode)
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

;; TODO: Возможно, заменить пакетом, основанным на tree-sitter ?

(use-package electric-pair
  :hook
  (after-init . electric-pair-mode)
  (emacs-lisp . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))
;; 
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

;;;; Подсветка глубины идентации

(use-package highlight-indent-guides
  :ensure t
  :custom
  (highlight-indent-guides-method 'character)
  :hook
  (css-base-mode . highlight-indent-guides-mode)
  (python-mode . highlight-indent-guides-mode)
  (yaml-ts-mode . highlight-indent-guides-mode))

;;;; Идентификаторы

;; Удобно подсветить разные идентификаторы разными цветами

(use-package color-identifiers-mode
  :if  window-system
  :defines (color-identifiers:modes-alist color-identifiers:re-not-inside-class-access)
  :ensure t
  :hook ((typescript-ts-mode . color-identifiers-mode)
       (js-ts-mode . color-identifiers-mode))
  :custom ((color-identifiers-coloring-method
           'hash)
          (color-identifiers:num-colors 16)
          (color-identifiers:color-luminance 0.3)
          (color-identifiers:min-color-saturation 0.2)
          (color-identifiers:max-color-saturation 0.7))
  :config
  (add-to-list
   'color-identifiers:modes-alist
   '(js-ts-mode "" "\\_<\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)"
                (nil font-lock-variable-name-face tree-sitter-hl-face:variable)))
  )

;; Альтернативный алгоритм подсветки идентификаторов

(use-package rainbow-identifiers
  :if window-system
  :ensure t
  :defer t
  :hook ((emacs-lisp-mode . rainbow-identifiers-mode)))

;;;; Форматирование

(use-package format-all
  :ensure t
  :hook ((ess-r-mode . format-all-mode)
       (python-mode . format-all-mode)
       ;; (emacs-lisp-mode . format-all-mode)
       (format-all-mode-hook . format-all-ensure-formatter))
  :config (custom-set-variables '(format-all-formatters (quote (("Python" black)
                                                               ("R" styler))))))

(use-package apheleia
  :ensure t
  :hook ((js-ts-mode . apheleia-mode)
       (typescript-ts-mode . apheleia-mode))
  :config)

;;;; Подсветка цветов

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode))

;;;; Статическая проверка кода

(use-package flymake
  :ensure t
  :custom ((flymake-no-changes-timeout 0.01)
          (elisp-flymake-byte-compile-load-path load-path))
  :hook (emacs-lisp-mode . flymake-mode)
  :bind (:map flymake-mode-map
                ("M-]" . flymake-goto-next-error)
                ("M-[" . flymake-goto-prev-error)
                ("M-\\" . flymake-show-buffer-diagnostics))
  
  
  )

;;;; Дебаггер

(require 'projectile)

(use-package dape
  :ensure t
  :init
  :defines (dape-buffer-window-arrangment dape-cwd-fn)
  :config
  (setq dape-buffer-window-arrangement 'gud)

  (setq dape-key-prefix "C-.")

  (add-hook 'dape-on-start-hooks
           (defun dape--save-on-start ()
             (save-some-buffers t t)))

  (setq dape-cwd-fn (lambda (&optional skip-tramp-trim)
                     (let ((root (projectile-project-root)))
                       (if (and (not skip-tramp-trim) (tramp-tramp-file-p root))
                           (tramp-file-name-localname (tramp-dissect-file-name root))
                         root)))))


;;;; Конструктор регулярных выражений

;; Замечательный инструмент, позволяющий визуально контролировать создание поисковых выражений

(загрузить 're-builder)

;; Используем синтаксис string (так меньше экранировать спец. символы)

(setq reb-re-syntax 'read)

;;;; Свёртка кода

(use-package origami
  :ensure t
  :bind
  (:map origami-mode-map
          ("C-<tab>" . origami-recursively-toggle-node)
          ("C-TAB" . origami-recursively-toggle-node)
          ("C-S-<tab>" . origami-recursively-toggle-node)
          ("C-M-<tab>" . origami-toggle-all-nodes)))

(provide 'про-код)
;;; про-код.el ends here
