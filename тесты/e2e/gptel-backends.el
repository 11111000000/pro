;;; gptel-backends.el --- E2E тест: проверка видимости всех gptel backends

;;; Commentary:
;; Surface: Integration.Ai
;; Stability: EXPERIMENTAL
;; Usage: emacs --batch -l тесты/e2e/gptel-backends.el

(add-to-list 'load-path (expand-file-name "интеграция" default-directory))
(add-to-list 'load-path (expand-file-name "тесты/unit" default-directory))

(require 'про-ии-ядро)

(message "=== Testing gptel backends visibility ===")

(unless про-ии--gptel-available
  (message "WARNING: gptel not available - checking if keys load anyway")

  (message "=== Checking loaded keys (from variables) ===")
  (message "  AITunnel key: %s" (if pro-ai-gptel-aitunnel-key "LOADED" "MISSING"))
  (message "  ProxyAPI key: %s" (if pro-ai-gptel-proxyapi-key "LOADED" "MISSING"))
  (message "  OpenRouter key: %s" (if pro-ai-gptel-openrouter-api-key "LOADED" "MISSING"))
  (message "  DeepSeek key: %s" (if pro-ai-gptel-deepseek-key "LOADED" "MISSING"))
  (message "  SiliconFlow key: %s" (if (про-ии--load-key-from-authinfo "api.siliconflow.com" "token") "LOADED" "MISSING"))
  (message "  Perplexity key: %s" (if (про-ии--load-key-from-authinfo "api.perplexity.ai" "token") "LOADED" "MISSING"))
  (message "  Chutes key: %s" (if (про-ии--load-key-from-authinfo "llm.chutes.ai" "token") "LOADED" "MISSING"))

  (message "=== Test complete (gptel not installed) ===")
  (kill-emacs 0))

(when про-ии--gptel-available
  (let (        (aitunnel-key (про-ии--load-key-from-authinfo "api.aitunnel.ru" "token"))
                (proxyapi-key (про-ии--load-key-from-authinfo "api.proxyapi.ru" "token"))
                (openrouter-key (про-ии--load-key-from-authinfo "openrouter.ai" "token"))
                (deepseek-key (про-ии--load-key-from-authinfo "api.deepseek.com" "token"))
                (siliconflow-key (про-ии--load-key-from-authinfo "api.siliconflow.com" "token"))
                (perplexity-key (про-ии--load-key-from-authinfo "api.perplexity.ai" "token"))
                (chutes-key (про-ии--load-key-from-authinfo "llm.chutes.ai" "token")))

    (message "  AITunnel key: %s" (if aitunnel-key "LOADED" "MISSING"))
    (message "  ProxyAPI key: %s" (if proxyapi-key "LOADED" "MISSING"))
    (message "  OpenRouter key: %s" (if openrouter-key "LOADED" "MISSING"))
    (message "  DeepSeek key: %s" (if deepseek-key "LOADED" "MISSING"))
    (message "  SiliconFlow key: %s" (if siliconflow-key "LOADED" "MISSING"))
    (message "  Perplexity key: %s" (if perplexity-key "LOADED" "MISSING"))
    (message "  Chutes key: %s" (if chutes-key "LOADED" "MISSING")))

  (message "=== Backends with models ===")
  (dolist (backend gptel--known-backends)
    (let* ((name (car backend))
           (backend-obj (gptel-get-backend name))
           (models (when backend-obj (gptel-backend-models backend-obj))))
      (when models
        (message "  %s: %d models" name (length models))
        (dolist (m (seq-take models 3))
          (message "    - %s" (gptel--model-name m))))))

  (message "=== Default backend ===")
  (when (bound-and-true-p gptel-backend)
    (message "  Backend: %s" (gptel-backend-name gptel-backend)))
  (when (bound-and-true-p gptel-model)
    (message "  Model: %s" (gptel--model-name gptel-model)))

  (message "=== Test complete ==="))
