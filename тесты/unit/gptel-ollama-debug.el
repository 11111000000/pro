;;; gptel-ollama-debug.el --- Debug gptel-Ollama integration issues -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Diagnose curl error 35 when using gptel with local Ollama backend.
;; Run with:
;;   emacs --batch -Q -l интеграция/про-ии-ядро.el -l тесты/unit/gptel-ollama-debug.el
;;
;; Or directly:
;;   emacs-headless-test тесты/unit/gptel-ollama-debug.el

;;; Code:

(add-to-list 'load-path (expand-file-name "интеграция" default-directory))
(add-to-list 'load-path (expand-file-name "тесты/unit" default-directory))

;; Load AI system which registers Ollama backend
(require 'про-ии-ядро)

(require 'url)
(require 'json)

(message "=== Testing Ollama connectivity ===")

;; Test 1. Direct HTTP request
(message "=== Test 1: Direct HTTP request ===")
(let ((payload (list :model "codellama:7b-instruct-q4_K_M"
                     :messages (list (list :role "user" :content "ping"))
                     :stream nil)))
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
        (buf (url-retrieve-synchronously "http://localhost:11434/api/chat/completions" t nil 10)))
    (message "Direct request result: %s" buf)
    (when buf
      (with-current-buffer buf
        (message "Response head: %s" (buffer-substring-no-properties (point-min) (min (+ (point-min) 200) (point-max))))))))

;; Test 2. Via gptel (if available)
(message "=== Test 2: gptel backend call ===")
(when (require 'gptel nil t)
  (let ((ollama-backend (gptel-get-backend "Ollama")))
    (if (not ollama-backend)
        (message "❌ Ollama backend not found!")
      (message "✅ Ollama backend registered"))))

(message "=== Debug complete ===")

(provide 'gptel-ollama-debug)
;;; gptel-ollama-debug.el ends here