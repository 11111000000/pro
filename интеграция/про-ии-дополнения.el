;;; про-ии-дополнения.el --- AI completion integrations -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Слой AI-дополнений: completion, editor-assist и локальные модели.

;;; Code:

(defvar про-ии-дополнения--initialized nil
  "Non-nil when the AI completion layer has been loaded.")

(setq про-ии-дополнения--initialized t)

(use-package minuet
  :ensure t
  :defines (minuet-active-mode-map
            minuet-provider
            minuet-openai-fim-compatible-options
            minuet-auto-suggestion-mode
            minuet-openai-compatible-options
            minuet-auto-suggestion-throttle-delay
            minuet-auto-suggestion-debounce-delay
            minuet-request-timeout)
  :functions (minuet-complete-with-minibuffer
              minuet-auto-suggestion-mode
              minuet-show-suggestion
              minuet-configure-provider
              minuet-previous-suggestion
              minuet-next-suggestion
              minuet-accept-suggestion
              minuet-accept-suggestion-line
              minuet-dismiss-suggestion
              minuet-set-optional-options)
  :bind (("s-i y" . minuet-complete-with-minibuffer)
         ("s-i TAB" . minuet-show-suggestion)
         ("s-i s-<tab>" . minuet-complete-with-minibuffer)
         ("s-i s-m" . minuet-configure-provider)
         :map minuet-active-mode-map
         ("M-p" . minuet-previous-suggestion)
         ("M-n" . minuet-next-suggestion)
         ("RET" . minuet-accept-suggestion)
         ("C-RET" . minuet-accept-suggestion)
         ("M-RET" . minuet-accept-suggestion-line)
         ("C-g" . minuet-dismiss-suggestion)
         ("M-q" . minuet-dismiss-suggestion)
         ("s-q" . minuet-dismiss-suggestion)
         ("M-a" . minuet-accept-suggestion-line))
  :custom ((minuet-context-window 16000)
           (minuet-request-timeout 3)
           (minuet-context-ratio 0.75))
  :config
  (when (boundp 'pro-ai-proxyapi-host)
    (plist-put minuet-openai-options :model "gpt-4-turbo")
    (setopt minuet-provider 'openai-compatible)
    (minuet-auto-suggestion-mode -1)
    (plist-put minuet-openai-compatible-options
               :end-point (concat "https://" pro-ai-proxyapi-host "/openai/v1/chat/completions"))
    (plist-put minuet-openai-compatible-options :api-key "OPENAI_API_KEY")
    (plist-put minuet-openai-compatible-options :model "gpt-4-turbo")
    (minuet-set-optional-options minuet-openai-compatible-options :provider nil)
    (minuet-set-optional-options minuet-openai-compatible-options :max_tokens nil)
    (minuet-set-optional-options minuet-openai-compatible-options :max_completion_tokens 512)
    (minuet-set-optional-options minuet-openai-compatible-options :top_p nil)))

(use-package evedel
  :ensure t
  :custom
  ((evedel-empty-tag-query-matches-all nil)
   (e-descriptive-mode-roles
    '((emacs-lisp-mode . "an Emacs Lisp programmer")
      (js-mode . "a JavaScript programmer")
      (js-ts-mode . "a JavaScript programmer")
      (typescript-ts-mode . "a TypeScript programmer")
      (typescript-mode . "a TypeScript programmer")
      (haskell-ts-mode . "a Haskell programmer")
      (bash-ts-mode . "a Bash programmer")
      (c-mode . "a C programmer")
      (c++-mode . "a C++ programmer")
      (lisp-mode . "a Common Lisp programmer")
      (web-mode . "a web developer")
      (erlang-mode . "an Erlang programmer")))))

(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "Russian")
  (setopt ellama-provider
          (make-llm-ollama :chat-model "codellama"
                           :embedding-model "codellama")))

(use-package codeium
  :init (when (fboundp 'установить-из)
          (установить-из :repo "Exafunction/codeium.el"))
  :functions (codeium-utf8-byte-length codeium-init codeium-completion-at-point)
  :bind (("C-c <tab>" . про-ии-дополнение-codeium))
  :config
  (defun про-ии-codeium-on ()
    "Включить Codeium."
    (interactive)
    (codeium-init)
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))
  (defun про-ии-codeium-off ()
    "Отключить Codeium."
    (interactive)
    (setq completion-at-point-functions
          (remove #'codeium-completion-at-point completion-at-point-functions)))
  (defun про-ии-codeium-toggle ()
    "Переключить состояние Codeium."
    (interactive)
    (if (memq #'codeium-completion-at-point completion-at-point-functions)
        (про-ии-codeium-off)
      (про-ии-codeium-on)))
  (defalias 'про-ии-дополнение-codeium (cape-capf-interactive #'codeium-completion-at-point))
  (setq use-dialog-box nil)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

(provide 'про-ии-дополнения)
;;; про-ии-дополнения.el ends here
