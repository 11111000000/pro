;; Debug OpenRouter API call
(message "=== Starting OpenRouter debug session ===")

;; Add current directory to load path
(add-to-list 'load-path "/home/zoya/pro")

;; Load required packages
(require 'auth-source)
(require 'url)

;; Load our package
(load-file "/home/zoya/pro/про-ии-ядро.el")

(message "Loaded про-ии-ядро")

;; Test key loading
(let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
  (if key
      (message "✓ OpenRouter API key loaded successfully")
    (message "✗ Failed to load OpenRouter API key")))

(message "=== OpenRouter debug session completed ===")
