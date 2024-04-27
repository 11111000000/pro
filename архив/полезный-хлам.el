;; (defun skewer-location-reload () (interactive) (skewer-eval "window.location.reload()"))

;; (use-package skewer-mode
;;   :ensure t
;;   :bind (("C-c r" . skewer-location-reload)))

;; (use-package prettier-js
;;   :ensure t
;;   :hook ((js-mode . prettier-js-mode)
;;          (web-mode . prettier-js-mode)
;;          (css-mode . prettier-js-mode)
;;          (json-mode . prettier-js-mode)
;;          (typescript-mode . prettier-js-mode)))

;; (defun use-eslint-from-node-modules ()
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/eslint/bin/eslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-javascript-eslint-executable eslint))))

;; (add-hook 'rjsx-mode-hook #'use-eslint-from-node-modules)


;; (require 'flymake-quicklintjs)

;; (defun попробовать-flymake-quicklintjs ()
;;   "Настройка `quicklintjs' c `flymake'."
;;   (unless (bound-and-true-p flymake-mode)
;;     (flymake-mode))
;;   (add-hook 'flymake-diagnostic-functions #'flymake-quicklintjs nil t)
;;   (setq-local flymake-no-changes-timeout 0)
;;   (message "Flymaje QuicklintJS")
;;   )

;; (add-hook 'js-ts-mode-hook #'попробовать-flymake-quicklintjs)
;; (add-hook 'typescript-ts-mode-hook #'попробовать-flymake-quicklintjs)

;; (use-package eslint-fix
;;   :hook (typescript-ts-mode . (lambda ()
;;                               (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json")))
;;                                 (setq-local eslint-fix-executable (file-name-concat root "node_modules" ".bin/eslint")))))
;;   :ensure t
;;   :init
;;   )


;; (require 'eglot-quicklintjs)

;; (use-package eglot-quicklintjs
;;   :init)


;; (use-package tide
;;   :ensure t
;;   :after (flycheck)
;;   :hook ((typescript-ts-mode . tide-setup)
;;        (tsx-ts-mode . tide-setup)
;;        (typescript-ts-mode . tide-hl-identifier-mode)
;;        (before-save . tide-format-before-save)))

;; (use-package tsi
;;   :ensure t
;;   :after tree-sitter
;;   ;;:quelpa (tsi :fetcher github :repo "orzechowskid/tsi.el")
;;   ;; define autoload definitions which when actually invoked will cause package to be loaded
;;   :commands (tsi-typescript-mode tsi-json-mode tsi-css-mode)
;;   :init
;;   (add-hook 'typescript-mode-hook (lambda () (tsi-typescript-mode 1)))
;;   (add-hook 'json-mode-hook (lambda () (tsi-json-mode 1)))
;;   (add-hook 'css-mode-hook (lambda () (tsi-css-mode 1)))
;;   (add-hook 'scss-mode-hook (lambda () (tsi-scss-mode 1))))

;; (use-package ts-comint
;;   :ensure t
;;   :bind (:map typescript-mode-map
;;               ( "C-x C-e" . ts-send-last-sexp)
;;               ( "C-c C-c" . ts-send-buffer)))
;;;; Подхватывать настройки проверки кода Javascript

;; ...Неверно работает модуль - сразу eslint fix вешает, убрать его...

;; (use-package eslint-rc
;;   :ensure t
;;   :custom ((eslint-rc-use-package-json t)
;;           (eslint-rc-use-eslintignore t)
;;           (eslint-rc-use-node-modules-bin t))

;;   :hook ((typescript-ts-mode . eslint-rc-mode)
;;        (tsx-ts-mode . eslint-rc-mode)
;;        (js-mode . eslint-rc-mode)
;;        (web-mode . eslint-rc-mode)))


;;;; Минибуфер во фрейме поверх окна

;; (use-package mini-frame
;;   :ensure t
;;   :custom
;;   (mini-frame-show-parameters '((child-frame-border-width . 0)
;;                                 (internal-border-width . 0)
;;                                 (top . nil)
;;                                 (width . 0.8)
;;                                 (height . 0.22)
;;                                 (left . 0.5)))
;;   (mini-frame-standalone t)
;;   (mini-frame-resize nil)
;;   (mini-frame-color-shift-step -7)
;;   (mini-frame-detach-on-hide nil)
;;   (mini-frame-internal-border-color "#333333")
;;   (mini-frame-ignore-commands '(eval-expression "edebug-eval-expression" debugger-eval-expression replace-string replace-regex))
;;   :config
;;   (mini-frame-mode t))

;; (use-package project-tab-groups
;;   :ensure
;;   :config
;;   (project-tab-groups-mode 1))

;; TODO Рамки окон...
;; (modify-all-frames-parameters
;;  '((right-divider-width . 40)
;;    (internal-border-width . 40)))
;; (dolist (face '(window-divider
;; 		      window-divider-first-pixel
;; 		      window-divider-last-pixel))
;;   (face-spec-reset-face face)
;;   (set-face-foreground face (face-attribute 'default :background)))
;; (set-face-background 'fringe (face-attribute 'default :background))
;; Шрифт минибуфера

;; (add-hook 'minibuffer-setup-hook 'my-minibuffer-setup)
;; (defun my-minibuffer-setup ()
;;        (set (make-local-variable 'face-remapping-alist)
;;             '((default :height 1.2))))

;; (dolist (buf '(" *Echo Area 0*" " *Echo Area 1*"))
;;   (with-current-buffer (get-buffer buf)
;;     (make-local-variable 'face-remapping-alist)
;;     (add-to-list 'face-remapping-alist '(default (:background "white" :height 1.3)))))
;;;;; Иконки для режимов
;; TODO: Настроить с taoline

;; (use-package mode-icons
;;   :ensure t
;;   :custom (
;;           (mode-icons-desaturate-active t))
;;   :config
;;   ;; В новых версиях Emacs есть два режима ELisp\d и ELisp\l - для динамической и лексической области видимости соотв.
;;   (add-to-list 'mode-icons  '(".*ELisp.*" "emacs" xpm))
;;   (add-to-list 'mode-icons  '(".*EXWM.*" #xf1b2 FontAwesome))
;;   (add-to-list 'mode-icons  '(".*Dired.*" #xf07c FontAwesome))
;;   (add-to-list 'mode-icons  '(".*TypeScript.*" #xeb1b FontAwesome))
;;   (add-to-list 'mode-icons  '(".*HTML.*" "html" xpm))
;;   (add-to-list 'mode-icons  '(".*Less.*" #xf1c9 FontAwesome))
;;   (mode-icons-mode t))

;;(blink-cursor-mode nil)
;;(setq x-stretch-cursor 1)
;;(setq blink-cursor-delay 0.3)

;;;;; Быстрая прокрутка

;; (require 'flycheck)

;; (use-package fast-scroll
;;   :defer 1
;;   :hook
;;   (fast-scroll-start . (lambda () (flycheck-mode -1)))
;;   (fast-scroll-end . (lambda () (flycheck-mode 1)))
;;   :config
;;   (fast-scroll-config)
;;   (fast-scroll-mode 1))

;;;;; Плавная прокрутка

;; (when (fboundp 'pixel-scroll-mode)
;;   (pixel-scroll-mode t))

;;;; HTTP-запросы

;; (use-package plz
;;   :init (установить-из :repo "alphapapa/plz")
;;   ;; :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
;;   )

;; (use-package ement
;;   :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

;; (use-package codeium-diagnose)
;;   :init
;;   (установить-из :repo "Exafunction/codeium-diagnose.el")
;;   :config
;;   (setq use-dialog-box nil)

;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t
;;   :custom ((copilot-node-executable "/usr/bin/node" "Set node executable.")
;;           (copilot-indent-warning-suppress t))
;;   :hook (prog-mode . copilot-mode)
;;   :config  
;;   (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;;   (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;;   ;; custom completion
;;   (with-eval-after-load 'copilot
;; 	(define-key copilot-mode-map (kbd "<tab>") #'gf3/copilot-tab)))

;; (use-package flycheck
;;   :bind (:map flycheck-mode-map
;;                 ("M-]" . flycheck-next-error)
;;                 ("M-[" . flycheck-previous-error)
;;                 ("M-\\" . flycheck-list-errors))
;;   :functions (global-flycheck-mode)
;;   :init
;;   (global-flycheck-mode t)
;;   )

;; (use-package flycheck-eglot
;;   :ensure t
;;   :after (flycheck eglot)
;;   :functions (global-flycheck-eglot-mode)
;;   :config
;;   (global-flycheck-eglot-mode t))

;;;; Сообщения статического анализатора во всплывающем окне

;; (use-package emacs-flymake-popon
;;   :init
;;   (unless (package-installed-p 'emacs-flymake-popon)
;;     (package-vc-install "https://codeberg.org/akib/emacs-flymake-popon.git"))
;;   :custom ((flymake-popon-delay .8)
;;           (flymake-popon-posframe-extra-arguments '(:poshandler posframe-poshandler-point-bottom-left-corner)))
;;   :hook ((flymake-mode) . flymake-popon-mode))

;; (use-package flymake-posframe
;;   :load-path "emacs-lisp/flymake-posframe"
;;   :hook (flymake-mode . flymake-posframe-mode))

;; (use-package flymake-diagnostic-at-point
;;   :ensure t
;;   :after flymake
;;   :config
;;   (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode))

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

;;;; Рефакторинг Emacs lisp

;; (use-package erefactor
;;   :ensure t
;;   :after (flymake))

;;;; Редактирование лиспа

;; (use-package lispy
;;   :ensure t
;;   ;; :hook
;;   ;; (emacs-lisp-mode . lispy-mode)
;;   )

;;;; Подсветка вызовов функций

;; (use-package highlight-function-calls
;;   :ensure t
;;   :hook
;;   (emacs-lisp-mode . highlight-function-calls-mode))

;;;; xref-find-difinition должен заходить в сжатые файлы


;; (defun etags-file-or-compressed-file-for (fname)
;;   "Return the valid file name for FNAME.
;; Check if FNAME is an existing file name, if not
;; try FNAME appended with the following compression extensions:
;; - \".gz\", the extension of compressed files created by gzip
;; - \".bz2\", the extension for compressed files created by bzip2
;; - \".xz\", the extension for compressed files created by xz
;; - \".lzma\", the extension for compressed files created by xz.

;; Return the file that exists or nil if nothing found."
;;   (let ((fpath nil))
;;     (cl-dolist (ext '(""
;;                       ".gz"
;;                       ".bz2"
;;                       ".xz"
;;                       ".lzma"))
;;       (setq fpath (concat fname ext))
;;       (when (file-exists-p fpath)
;;         (cl-return fpath)))))

;; (cl-defmethod xref-location-marker ((l xref-etags-location))
;;   (with-slots (tag-info file) l
;;     (let (buffer
;;          (fname (pel-file-or-compressed-file-for file)))
;;       (if fname
;;           (setq buffer (find-file-noselect fname))
;;         (user-error "file %s (or .gz, .bz2, .xz, .lzma) does not exist" file))
;;       (with-current-buffer buffer
;;         (save-excursion
;;           (etags-goto-tag-location tag-info)
;;           (point-marker))))))
;; (use-package fixed-pitch
;;   :ensure t
;;   :init (установить-из :repo "cstby/fixed-pitch-mode")
;;   :config
;;   (fixed-pitch-mode))


  ;; (let* ((variable-tuple (cond
  ;;                        ;; ((x-list-fonts "OldSlavic")         '(:font "OldSlavic"))
  ;;                        ((x-list-fonts "Golos")         '(:font "Golos"))
  ;;                        ((x-list-fonts "DejaVu")         '(:font "DejaVu Sans"))
  ;;                        ((x-list-fonts "Scientia")         '(:font "Scientia"))
  ;;                        ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;                        ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;                        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;                        ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;                        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;                        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;       (base-font-color     (face-foreground 'default nil 'default))
  ;;       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;                     '(variable-pitch ((t (:family "Golos")))))
  
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
  ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1 :weight normal))))
  ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1 :weight normal))))
  ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2 :weight normal))))
  ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.3 :weight normal))))
  ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.1 :underline nil :weight normal)))))
  ;;                     :family "DejaVu Sans"
  ;;                     :weight 'normal
  ;;                     :height 160)
  ;; (set-face-attribute 'fixed-pitch nil
  ;;                     :family "Fira Code"
  ;;                     :weight 'normal)
  ;; (set-face-attribute 'variable-pitch nil
  ;;                     :family "DejaVu Sans"
  ;;                     :weight 'normal)
