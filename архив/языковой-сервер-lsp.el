;;; Языковой Сервер (LSP)

(use-package lsp-mode 
  :ensure t 
  :after tree-sitter 
  :commands (lsp lsp-deferred)
  ;;:hook (
           ;; (typescript-mode . lsp-deferred)
         ;; (typescript-mode . lsp-ui-mode) 
         ;; (typescript-mode . lsp-completion-mode)
         ;; (typescript-mode . dap-mode)
         ;; (typescript-mode . dap-ui-mode)

  :custom ((lsp-keymap-prefix "C-c o") 
           (lsp-before-save-edits            t) 
           (lsp-eldoc-render-all             nil) 
           (lsp-eldoc-hook                   nil) 
           (lsp-highlight-symbol-at-point    nil) 
           (lsp-log-io                       nil) 
           (lsp-print-performance            nil) 
           (lsp-inhibit-message              t) 
           (lsp-report-if-no-buffer          nil) 
           (lsp-keep-workspace-alive         t) 
           (lsp-enable-snippet               t) 
           (lsp-signature-auto-activate      nil) 
           (lsp-restart                      'interactive) 
           (lsp-auto-configure               t) 
           (lsp-auto-guess-root              t) 
           (lsp-document-sync-method         lsp--sync-incremental) 
           (lsp-auto-execute-action          nil) 
           (lsp-enable-xref                  t) 
           (lsp-enable-indentation           nil) 
           (lsp-enable-imenu                 t) 
           (lsp-prefer-flymake               nil) 
           (lsp-enable-on-type-formatting    nil) 
           (lsp-signature-auto-activate      nil) 
           (lsp-completion-enable            t) 
           (lsp-lens-enable                  nil) 
           (lsp-modeline-diagnostics-enable  t) 
           (lsp-eslint-enable                t) 
           (lsp-completion-provider          :capf) 
           (lsp-completion-show-kind         t) 
           (lsp-imenu-sort-methods '(position)) 
           (lsp-enable-semantic-highlighting t) 
           (lsp-enable-symbol-highlighting nil) 
           (lsp-idle-delay 0.5) 
           (lsp-log-io nil) 
           (lsp-use-plists t)) 
  :bind (:map lsp-mode-map
              ("M-." . lsp-find-definition) 
              ("C-M-." . lsp-find-implementation)) 
  :config (setq read-process-output-max (* 1024 1024))
  ;; (setq gc-cons-threshold 100000000)
  (setenv "LSP_USE_PLISTS" "true") 
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\node_modules\\'")
  ;; (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.git\\'")
  ;; (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")
  :init (require 'lsp-javascript) 
  (require 'lsp-html) 
  (require 'lsp-css) 
  (require 'lsp-json) 
  (require 'lsp-eslint) 
  (require 'lsp-xml) 
  (require 'lsp-yaml))

;;;;; LSP UI

(use-package lsp-ui 
  :ensure t 
  :bind (:map lsp-ui-mode-map
              ("M-m" . lsp-ui-imenu)) 
  :custom ((lsp-ui-doc-enable t) 
           (lsp-ui-doc-include-signature t) 
           (lsp-ui-doc-show-with-cursor t) 
           (lsp-ui-doc-header t) 
           (lsp-ui-doc-position 'top) 
           (lsp-ui-doc-max-width 90) 
           (lsp-ui-doc-max-height 30) 
           (lsp-ui-doc-use-childframe t) 
           (lsp-ui-doc-use-webkit nil) 
           (lsp-ui-doc-delay 1.5) 
           (lsp-ui-imenu-enable t) 
           (lsp-treemacs-sync-mode t) 
           (lsp-ui-flycheck-enable t) 
           (lsp-ui-flycheck-list-position 'right) 
           (lsp-ui-flycheck-live-reporting t) 
           (lsp-ui-peek-always-show t) 
           (lsp-ui-peek-enable t) 
           (lsp-ui-peek-list-width 60) 
           (lsp-ui-sideline-enable nil) 
           (lsp-ui-sideline-show-diagnostics t) 
           (lsp-ui-sideline-show-symbol t) 
           (lsp-ui-sideline-show-hover t) 
           (lsp-ui-sideline-show-code-actions t) 
           (lsp-ui-sideline-ignore-duplicate t) 
           (lsp-ui-sideline-update-mode 'point) 
           (lsp-ui-doc-border (face-attribute 'default 
                                              :foreground))) 
  :config
  ;;(flycheck-add-next-checker 'lsp-ui 'typescript-tslint)
  (custom-set-faces `(lsp-ui-doc-background 
                      ((t 
                        :height 95 
                        :background ,(face-attribute 'default 
                                                     :background)))) 
                    `(lsp-ui-doc 
                      ((t 
                        :height 95))) 
                    `(lsp-ui-doc-header 
                      ((t 
                        :height 95 
                        :foreground ,(face-attribute 'default 
                                                     :background) 
                        :background ,(face-attribute 'default 
                                                     :foreground)))) 
                    `(lsp-ui-doc-url 
                      ((t 
                        :height 95 
                        :inherit link)))) 
  (add-hook 'lsp-ui-doc-frame-hook 
            (lambda 
              (frame _w) 
              (set-face-attribute 'default frame 
                                  :font "Fira Code" 
                                  :height 80))))

;;;;; LSP Treemacs

(use-package lsp-treemacs 
  :ensure t 
  :bind (:map lsp-mode-map
              ("M-M" . lsp-treemacs-symbols)) 
  :commands lsp-treemacs-errors-list 
  :config)

;;; Sonar lint

(use-package lsp-sonarlint 
  :ensure t 
  :custom ((lsp-sonarlint-typescript-enabled t) 
           (lsp-sonarlint-javascript-enabled t)) 
  :config (require 'lsp-sonarlint-typescript) 
  (require 'lsp-sonarlint-javascript))

;;; LSP Docker

(use-package lsp-docker 
  :ensure t)




(provide 'dobro-code-lsp)

;;; dobro-code-lsp.el ends here
