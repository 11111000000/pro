;;; искусственный-интеллект.el --- Искусственный Интеллект -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)
;; Version: 1.0
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/11111000000/pro
;;; Commentary:
;; Конфигурация нейросетевых сервисов

;;; Code:
;;;; LLAMA

(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "English")
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

(defun запустить-codeium()
  "Enable codeium."
  (interactive)
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

(use-package codeium
  :disabled t
  :ensure t
  :config
  (setq codeium/metadata/api_key
      (replace-regexp-in-string "\n\\'" ""
                      (shell-command-to-string "pass show services/codeium/iocanel/api-key-emacs")))

  (setq use-dialog-box nil) ;; do not use popup boxes
  
  ;; используйте M-x codeium-diagnose, чтобы увидеть API/поля, которые будут отправлены на локальный языковой сервер
  (setq codeium-api-enabled
      (lambda (api)
        (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; you can also set a config for a single buffer like this:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))

  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;; (use-package codeium-diagnose)
;;   :init
;;   (установить-из :repo "Exafunction/codeium-diagnose.el")
;;   :config
;;   (setq use-dialog-box nil)


(provide 'искусственный-интеллект)

;;; искусственный-интеллект.el ends here
