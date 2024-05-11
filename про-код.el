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
  :config
  
  )

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
  :hook (
       ;; (go-mode . eglot-ensure)
       ;; (haskell-mode . eglot-ensure)
       ;; (typescript-mode . eglot-ensure)
       ;; (rust-mode . eglot-ensure)
       
       (python-ts-mode . eglot-ensure)
       ;; (haskell-mode . eglot-ensure)
       ;;(js-mode . eglot-ensure)
       
       ;; (json-mode . eglot-ensure)
       ;; (rust-mode . eglot-ensure)
       )
  :functions (eglot-rename eglot-code-actions)
  :bind (:map eglot-mode-map
                ("C-c r" . eglot-rename)
                ("M-'" . eglot-inlay-hints-mode)
                ("C-<down-mouse-1>" . xref-find-definitions)
                ("M-." . xref-find-definitions)
                ("C-M-." . xref-find-references)
                ("C-S-<down-mouse-1>" . xref-find-references)
                ("C-c ." . eglot-code-actions)
                ("C-c C-c" . eglot-code-actions))
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 3)
  (eglot-events-buffer-size '(:size 2000 :format full))
  (eglot-send-changes-idle-time 0)
  :config
  
  ;; Выключим лог, что увеличивает производительность
  
  (fset #'jsonrpc--log-event #'always)

  ;; (add-hook 'focus-out-hook 'garbage-collect)
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
;; (use-package smartparens
;;   :ensure t
;;   :defines (smartparens-global-mode sp-local-pair)
;;   :bind  (("C-^" . sp-unwrap-sexp)
;;           ("M-j" . sp-next-sexp)
;;           ("M-k" . sp-backward-sexp)
;;           ("M-h" . sp-backward-up-sexp)
;;           ("M-l" . sp-down-sexp))
;;   :config

;;   (smartparens-global-mode 1)
;;   (sp-local-pair 'emacs-lisp-mode "'" nil
;;                  :actions nil)
;;   (sp-local-pair 'scheme-mode "'" nil
;;                  :actions nil)
;;   (sp-local-pair 'racket-mode "'" nil
;;                  :actions nil)
;;   (sp-local-pair 'lisp-mode "" nil
;;                  :actions nil)
;;   (show-smartparens-global-mode t)
;;   ;; (show-paren-mode -1)
;;   )

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
  :ensure t
  :hook ((typescript-ts-mode . color-identifiers-mode))
  :custom ((color-identifiers-coloring-method
           'hash)
          (color-identifiers:num-colors 16)
          (color-identifiers:color-luminance 0.3)
          (color-identifiers:min-color-saturation 0.2)
          (color-identifiers:max-color-saturation 0.7)))

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
  :hook ((js-mode . apheleia-mode)
       (typescript-ts-mode . apheleia-mode))
  :config
  ;;(apheleia-global-mode nil)
  )

;;;; Подсветка цветов

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode))

;;;; Статическая проверка кода

(use-package flymake
  :ensure t
  :custom ((flymake-no-changes-timeout 0.01))
  :hook ((emacs-lisp-mode) . flymake-mode)
  :bind (:map flymake-mode-map
                ("M-]" . flymake-goto-next-error)
                ("M-[" . flymake-goto-prev-error)
                ("M-\\" . flymake-show-buffer-diagnostics)))

;;;; Сниппеты

;; Быстрые шаблоны - сниппеты можно создавать на лету со шпаргалкой

(defvar шаблон-для-сниппета (concat
                             (file-name-directory
                              (locate-library "про-код"))
                             "etc/шаблон-сниппета.txt"))

(defun создать-новый-сниппет-со-шпаргалкой ()
  "Создать новый сниппет со шпаргалкой."
  (interactive)
  (funcall-interactively 'yas-new-snippet)
  (erase-buffer)
  (insert-file-contents шаблон-для-сниппета))

(use-package yasnippet
  :ensure t
  :functions (yas-reload-all)
  :defines (yas-snippet-dirs)
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
      '("~/.emacs.d/snippets")))

(use-package yasnippet-snippets
  :ensure t
  :init)

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
