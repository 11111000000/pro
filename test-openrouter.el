;;; test-openrouter.el --- Test OpenRouter integration

;; Load the core AI integration
(load-file "/home/zoya/pro/интеграция/про-ии-core.el")

(defun test-openrouter-key-loading ()
  "Test loading OpenRouter key from authinfo."
  (message "=== Testing OpenRouter Key Loading ===")
  (let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
    (if key
        (progn
          (message "✓ OpenRouter key loaded successfully")
          (message "  Key preview: %s" 
                   (if (> (length key) 20) 
                       (concat (substring key 0 10) "..." (substring key -10))
                     key))
          t)
      (message "✗ Failed to load OpenRouter key")
      nil)))

(defun test-openrouter-models-fetch ()
  "Test fetching OpenRouter models."
  (message "=== Testing OpenRouter Models Fetch ===")
  (let ((models (pro-ai-gptel-openrouter-free-models t)))
    (if models
        (progn
          (message "✓ Successfully fetched %d free models" (length models))
          (message "  First 5 models:")
          (dolist (model (seq-take models 5))
            (message "    - %s" model))
          t)
      (message "✗ Failed to fetch free models")
      nil)))

(defun test-openrouter-backend-registration ()
  "Test OpenRouter backend registration."
  (message "=== Testing OpenRouter Backend Registration ===")
  (if (bound-and-true-p про-ии--gptel-available)
      (let ((models (pro-ai-gptel-openrouter-free-models t)))
        (if models
            (progn
              (pro-ai-gptel--openrouter-set-backend models)
              (let ((backend (gptel-get-backend "Openrouter")))
                (if backend
                    (progn
                      (message "✓ OpenRouter backend registered successfully")
                      (let ((backend-models (gptel-backend-models backend)))
                        (message "  Backend has %d models" (length backend-models))
                        (when backend-models
                          (message "  First 3 model names:")
                          (dolist (model (seq-take backend-models 3))
                            (message "    - %s" (gptel--model-name model)))))
                      t)
                  (message "✗ Failed to register OpenRouter backend")
                  nil))
          (message "✗ No models available for backend registration")
          nil))
    (message "✗ gptel is not available")
    nil))

(defun run-all-tests ()
  "Run all OpenRouter integration tests."
  (message "=== OpenRouter Integration Tests ===")
  
  (let ((results '()))
    ;; Test 1: Key loading
    (push (cons "Key Loading" (test-openrouter-key-loading)) results)
    
    ;; Test 2: Models fetch
    (push (cons "Models Fetch" (test-openrouter-models-fetch)) results)
    
    ;; Test 3: Backend registration
    (push (cons "Backend Registration" (test-openrouter-backend-registration)) results)
    
    ;; Print summary
    (message "=== Test Results ===")
    (dolist (result results)
      (if (cdr result)
          (message "✓ %s: PASSED" (car result))
        (message "✗ %s: FAILED" (car result))))
    
    (let ((passed (length (cl-remove-if-not #'cdr results)))
          (total (length results)))
      (message "Overall: %d/%d tests passed" passed total)
      (if (= passed total)
          (message "🎉 All tests PASSED!")
        (message "❌ Some tests FAILED")))
    
    results))

;; Run the tests
(run-all-tests)