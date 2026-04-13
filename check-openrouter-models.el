(require 'про-ии-core)

(message "=== OpenRouter Models Test ===")

;; Load API key
(let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
  (if key
      (message "✓ API key loaded: %s..." (substring key 0 10))
    (message "✗ No API key found")))

;; Get free models
(let ((models (pro-ai-gptel-openrouter-free-models)))
  (message "Total free models: %d" (length models))
  (when (> (length models) 0)
    (message "First 10 models:")
    (dolist (model (seq-take models 10))
      (message "  - %s" model))
    
    ;; Check for specific models
    (let ((has-gemma (seq-find (lambda (m) (string-match "gemma" m)) models))
          (has-llama (seq-find (lambda (m) (string-match "llama" m)) models))
          (has-qwen (seq-find (lambda (m) (string-match "qwen" m)) models)))
      (when has-gemma (message "✓ Has Gemma models"))
      (when has-llama (message "✓ Has Llama models"))
      (when has-qwen (message "✓ Has Qwen models")))))