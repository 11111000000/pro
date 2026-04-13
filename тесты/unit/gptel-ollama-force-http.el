;;; gptel-ollama-force-http.el --- Force HTTP for localhost in gptel -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Try to force HTTP for localhost in gptel by modifying the struct
;; Run: nix run .#emacs-headless-test -- тесты/unit/gptel-ollama-force-http.el

;;; Code:

(message "=== Test: Modify gptel backend protocol to HTTP ===")

(require 'gptel)

(require 'gptel-ollama)

;; Turn off curl to use url-retrieve
(setq gptel-use-curl nil)

;; Register Ollama
(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :models '("codellama:7b-instruct-q4_K_M"))

;; Get backend and modify protocol
(let* ((backend-pair (assoc "Ollama" gptel--known-backends))
       (backend (cdr backend-pair)))
  (when backend
    (message "Original protocol: %s" (gptel-backend-protocol backend))
    (message "Original URL: %s" (gptel-backend-url backend))
    
    ;; Try to modify the protocol slot
    ;; gptel-openai struct has protocol at position 3 (0-indexed)
    (setf (cl-struct-slot-value 'gptel-openai 'protocol backend) "http")
    (message "Modified protocol: %s" (gptel-backend-protocol backend))
    (message "Modified URL: %s" (gptel-backend-url backend))

    ;; Now try request
    (setq gptel-backend backend)
    (setq gptel-model (car (gptel-backend-models backend)))
    (message "Model: %s" (gptel--model-name gptel-model))
    
    ;; Make request
    (message "Making request...")
    (gptel-request "ping" :system "ok")))

(message "=== Test complete ===")

(provide 'gptel-ollama-force-http)
;;; gptel-ollama-force-http.el ends here
