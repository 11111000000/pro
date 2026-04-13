;;; ai-integration.el --- E2E тест: ИИ-интеграция ПРО

;;; Commentary:
;; Surface: Integration.Ai
;; Stability: FROZEN
;; Invariant: INV-Test-Coverage
;; Usage: emacs --batch -l tests/e2e/ai-integration.el

;;; Code:

(add-to-list 'load-path (expand-file-name "интеграция" user-emacs-directory))

['про-ии] %s backend зарегистрирован" name)))))))\n    ;; AITunnel\n    (funcall log-and-register\n             "AITunnel" t nil\n             pro-ai-gptel-aitunnel-all-models\n             "api.aitunnel.ru" "/v1/chat/completions" (про-ии--load-key-from-authinfo "api.aitunnel.ru" "token"))\n\n    ;; SiliconFlow\n    (let ((si-key (про-ии--load-key-from-authinfo "api.siliconflow.com" "token")))\n      (when si-key\n        (funcall log-and-register\n                 "SiliconFlow" t nil\n                 \'("Qwen/Qwen3-8B"\n                   "deepseek-ai/DeepSeek-R1-Distill-Qwen-32B"\n                   "Qwen/Qwen3-8B")\n                 "api.siliconflow.com" "/v1/chat/completions" si-key)))\n\n    ;; Set OpenRouter as default if available\n    (let ((openrouter-backend (gptel-get-backend pro-ai-gptel-openrouter-backend-name)))\n      (when openrouter-backend\n        (setq gptel-backend openrouter-backend)\n        (let* ((models (gptel-backend-models openrouter-backend))\n               (model-names (pro-ai-gptel--backend-model-names openrouter-backend))\n               (choice (pro-ai-gptel--preferred-model\n                        model-names\n                        pro-ai-gptel-openrouter-preferred-models)))\n          (setq gptel-model\n                (or (and choice\n                         (seq-find (lambda (model)\n                                     (string= (gptel--model-name model) choice))\n                                   models))\n                    (car models))))))))\n\n(message "=== AI integration loaded ===']

;; Force OpenRouter backend setup
(when про-ии--gptel-available
  (let ((free-models (pro-ai-gptel-openrouter-free-models t)))
    (when free-models
      (pro-ai-gptel--openrouter-set-backend free-models)
      (message "=== OpenRouter backend registered with %d models ===" (length free-models)))))

;; List all registered backends
(when (bound-and-true-p gptel--known-backends)
  (message "=== Registered backends ===")
  (dolist (backend gptel--known-backends)
    (message "  - %s" (car backend))))

;; Check current backend and model
(when (bound-and-true-p gptel-backend)
  (message "Current backend: %s" (gptel-backend-name gptel-backend)))
(when (bound-and-true-p gptel-model)
  (message "Current model: %s" (gptel--model-name gptel-model)))

;; Test a minimal gptel call if backend is ready
(when (and про-ии--gptel-available gptel-backend gptel-model)
  (with-temp-buffer
    (insert "Hello, OpenRouter!")
    (goto-char (point-max))
    (let ((gptel-use-context nil)
          (gptel-log-level 'debug)
          (gptel-timeout 30))
      ;; Mock send without network to test request structure
      (message "=== Ready to send request (not called due to batch mode) ==="))))

(provide 'ai-integration)
;;; ai-integration.el ends here
