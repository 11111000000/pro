;; Test OpenRouter model requests
(require 'gptel)
(require 'auth-source)

;; Load OpenRouter key from authinfo
(setq openrouter-key (plist-get (car (auth-source-search :host "openrouter.ai" :user "token")) :secret))
(when (functionp openrouter-key)
  (setq openrouter-key (funcall openrouter-key)))

;; Test gptel with OpenRouter
(setq test-backend
      (gptel-make-openrouter "OpenRouter"
        :stream t
        :key openrouter-key
        :models (list "google/gemma-2-9b-it:free" "meta-llama/llama-3.1-8b-instruct:free")))

;; Test request
(with-temp-buffer
  (insert "Hello, world!")
  (let ((gptel-backend test-backend))
    (gptel-send (current-buffer))))