;;; про-искусственный-интеллект.el --- Искусственный Интеллект -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)
;;; Commentary:
;; Конфигурация нейросетевых сервисов
;;; Code:

;;;; OpenAI

(use-package openai
  :init
  (установить-из :repo "11111000000/openai")
  :custom
  (openai-base-url "https://api.proxyapi.ru/openai/v1"))


;; ChatGPT Shell

(use-package chatgpt-shell
  :ensure t
  :custom (
          (chatgpt-shell-api-url-base  "https://api.proxyapi.ru/openai")))

;;;; LLAMA

(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "English")
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

;;;; Codeium

(require 'установить-из)

(defun запустить-codeium()
  "Enable codeium."
  (interactive)
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(require 'cape)

(use-package codeium
  :init (установить-из :repo "Exafunction/codeium.el")
  :bind ("C-c <TAB>" . дополнить-codeium) ("M-S-<iso-lefttab>" . дополнить-codeium)
  :config
  (defalias 'дополнить-codeium
    (cape-capf-interactive #'codeium-completion-at-point))
  
  (setq use-dialog-box nil)
  
  ;; используй M-x codeium-diagnose, чтобы увидеть API/поля, которые будут отправлены на локальный языковой сервер
  
  (setq codeium-api-enabled
       (lambda (api)
         (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  
  ;; Для отдельного буфера можно настроить так:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  (setq codeium/document/text
       (lambda ()
         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max)))))
  (setq codeium/document/cursor_offset
       (lambda ()
         (codeium-utf8-byte-length
          (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))))




(provide 'про-искусственный-интеллект)

;;; про-искусственный-интеллект.el ends here
