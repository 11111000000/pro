;;; gptel-ollama-http-test.el --- Test Ollama with gptel-use-curl=nil -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Test if turning off curl and using url-retrieve fixes Ollama
;; Run: nix run .#emacs-headless-test -- тесты/unit/gptel-ollama-http-test.el

;;; Code:

(message "=== Test: Ollama with gptel-use-curl = nil ===")

(require 'gptel)

(require 'gptel-ollama)

;; Turn off curl, use url-retrieve
(setq gptel-use-curl nil)
(message "gptel-use-curl set to: %s" gptel-use-curl)

;; Register Ollama
(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :models '("codellama:7b-instruct-q4_K_M"))

(message "Backend registered")

;; Get backend
(let* ((backend-pair (assoc "Ollama" gptel--known-backends))
       (backend (cdr backend-pair)))
  (when backend
    (setq gptel-backend backend)
    (setq gptel-model (car (gptel-backend-models backend)))
    (message "gptel-model: %s" (gptel--model-name gptel-model))
    
    ;; Try request with dry-run first to see what would be sent
    (message "Testing dry-run...")
    (let ((data (gptel-request "ping" :system "Reply ok" :dry-run t)))
      (message "Dry-run data model: %s" (plist-get data :model))
      (message "Dry-run data URL would use: %s" (gptel-backend-url backend)))
    
    ;; Now make actual request - sync call
    (message "Making actual request...")
    (gptel-request "ping" :system "Reply ok")))

(message "=== Test complete ===")

(provide 'gptel-ollama-http-test)
;;; gptel-ollama-http-test.el ends here
