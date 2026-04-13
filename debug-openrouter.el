;; Debug script to check OpenRouter integration
(message "=== OpenRouter Integration Debug ===")

;; Load required libraries
(require 'url)
(require 'json)
(require 'auth-source)

;; Add integration path
(add-to-list 'load-path "/home/zoya/pro/интеграция")
(load-file "/home/zoya/pro/интеграция/про-ии-core.el")

(message "1. Testing key loading...")
(let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
  (if key
      (progn
        (message "   ✓ Key loaded successfully")
        (message "   Key preview: %s..." (substring key 0 15)))
    (message "   ✗ Failed to load key")))

(message "2. Testing free models fetch...")
(let ((models (pro-ai-gptel-openrouter-free-models t)))
  (if models
      (progn
        (message "   ✓ Fetched %d free models" (length models))
        (message "   First model: %s" (car models))
        (message "   Last model: %s" (car (last models))))
    (message "   ✗ Failed to fetch models")))

(message "3. Testing backend registration...")
(when (bound-and-true-p про-ии--gptel-available)
  (let ((models (pro-ai-gptel-openrouter-free-models t)))
    (if models
        (progn
          (pro-ai-gptel--openrouter-set-backend models)
          (let ((backend (gptel-get-backend "Openrouter")))
            (if backend
                (progn
                  (message "   ✓ Backend registered")
                  (let ((backend-models (gptel-backend-models backend)))
                    (message "   Backend has %d models" (length backend-models))
                    (when backend-models
                      (let ((current-model (or gptel-model (car backend-models))))
                        (when current-model
                          (message "   Current model: %s" (gptel--model-name current-model)))))))
              (message "   ✗ Backend registration failed"))))
      (message "   ✗ No models for registration"))))

(message "4. Testing model name parsing...")
(let ((test-models '("qwen/qwen3-coder:free" 
                    "google/gemma-4-26b-a4b-it:free" 
                    "nvidia/nemotron-3-super-120b-a12b:free")))
  (dolist (model test-models)
    (message "   Model: %s" model)))

(message "=== Debug completed ===")