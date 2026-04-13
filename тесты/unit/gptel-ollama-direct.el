;;; gptel-ollama-direct.el --- Direct test without про-ии-ядро -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Test gptel-Ollama integration directly from nix Emacs
;; Run: nix run .#emacs-headless-test -- тесты/unit/gptel-ollama-direct.el

;;; Code:

(message "=== Testing gptel Ollama registration and request ===")

;; Require gptel
(require 'gptel)

;; Load Ollama backend implementation
(require 'gptel-ollama)

;; Show gptel-request signature
(message "gptel-request docstring: %s" (documentation 'gptel-request))

;; Register Ollama backend
(message "Registering Ollama backend...")
(gptel-make-ollama "Ollama"
  :host "localhost:11434"  
  :models '("codellama:7b-instruct-q4_K_M" "codellama:latest"))

(message "Backend registered.")

;; Extract backend directly from alist (since gptel-get-backend is nil)
(let* ((backend-pair (assoc "Ollama" gptel--known-backends))
       (backend (cdr backend-pair)))
  (message "Ollama backend from alist: %s" (type-of backend))
  (when backend
    (let ((models (gptel-backend-models backend)))
      (message "Backend has %d models" (length models))
      (when models
        (setq gptel-backend backend)
        (setq gptel-model (car models))
        (message "gptel-model set to: %s" (gptel--model-name gptel-model))
        
        ;; Try to send a simple request - use :system keyword
        (message "Calling gptel-request with :system...")
        (gptel-request "ping" :system "You are a helpful assistant")))))

(message "=== Test complete ===")

(provide 'gptel-ollama-direct)
;;; gptel-ollama-direct.el ends here
