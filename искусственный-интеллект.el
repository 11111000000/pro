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



(provide 'искусственный-интеллект)

;;; искусственный-интеллект.el ends here
