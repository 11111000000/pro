;;; gptel-ollama-curl-test.el --- Test curl directly to find exit 35 -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Test Ollama curl request from nix Emacs to find exit 35 cause
;; Run: nix run .#emacs-headless-test -- тесты/unit/gptel-ollama-curl-test.el

;;; Code:

(message "=== Test 1: Direct curl to Ollama via shell ===")

;; Test 1: Direct shell curl
(let ((result (shell-command-to-string "curl -s --max-time 5 http://localhost:11434/api/tags")))
  (message "Shell curl result: %s" (if (string-match "^{" result) "OK (JSON)" result)))

(message "=== Test 2: Emacs url-retrieve to Ollama ===")

;; Test 2: Emacs url-retrieve (what gptel uses)
(require 'url)
(require 'json)

(let ((url-request-method "GET")
      (buf (url-retrieve-synchronously "http://localhost:11434/api/tags" t nil 5)))
  (if buf
      (with-current-buffer buf
        (message "url-retrieve status: %s" (buffer-string)))
    (message "url-retrieve failed")))

(message "=== Test 3: POST request to chat ===")

;; Test 3: POST to chat endpoint
(let* ((payload (json-encode (list :model "codellama:7b-instruct-q4_K_M"
                                   :messages (list (list :role "user" :content "ping"))
                                   :stream :json-false)))
       (url-request-method "POST")
       (url-request-extra-headers '(("Content-Type" . "application/json")))
       (url-request-data payload)
       (buf (url-retrieve-synchronously "http://localhost:11434/api/chat" t nil 30)))
  (if buf
      (with-current-buffer buf
        (message "POST response: %s" (buffer-substring-no-properties (point-min) (min 500 (point-max)))))
    (message "POST request failed")))

(message "=== Test 4: Check gptel-use-curl ===")

(require 'gptel)
(require 'gptel-ollama)
(message "gptel-use-curl value: %s" gptel-use-curl)
(message "gptel-backend-protocol exists: %s" (boundp 'gptel-backend-protocol))

(message "=== Test 5: Register Ollama and trace request ===")

;; Register without key
(gptel-make-ollama "Ollama"
  :host "localhost:11434"
  :models '("codellama:7b-instruct-q4_K_M"))

(message "Ollama backend in gptel--known-backends: %s" 
         (if (assoc "Ollama" gptel--known-backends) "YES" "NO"))

;; Check what URL gptel would construct
(let* ((backend (cdr (assoc "Ollama" gptel--known-backends)))
       (url (gptel-backend-url backend)))
  (message "gptel backend URL: %s" url))

(message "=== All tests complete ===")

(provide 'gptel-ollama-curl-test)
;;; gptel-ollama-curl-test.el ends here
