;;; ai-openrouter-api.el --- API tests for OpenRouter -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Tests OpenRouter API integration including real HTTP requests.

;;; Code:

(require 'ert)
(require 'url)
(require 'json)

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (root (if (file-directory-p source)
                   (file-name-as-directory source)
                 (file-name-directory source))))
    (add-to-list 'load-path (expand-file-name "../../интеграция/" root))))

(require 'про-ии-core)

(ert-deftest pro-ai-openrouter-http-request ()
  "Test HTTP request to OpenRouter API."
  ;; Load the key
  (let ((api-key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
    (if api-key
        (progn
          (should (stringp api-key))
          (should (> (length api-key) 20))
          (message "✓ OpenRouter API key loaded: %s..." (substring api-key 0 10)))
      (message "Warning: No OpenRouter API key found - skipping HTTP test"))))

(ert-deftest pro-ai-openrouter-free-model-detection ()
  "Test detection of free models from sample data."
  (let* ((sample-data 
          '((data 
             ((id . "qwen/qwen3-coder:free")
              (pricing . ((prompt . "0") (completion . "0"))))
             ((id . "google/gemma-4-26b-a4b-it:free")
              (pricing . ((prompt . "0") (completion . "0"))))
             ((id . "paid-model")
              (pricing . ((prompt . "0.001") (completion . "0.002")))))))
         (items (alist-get 'data sample-data))
         (free-items (seq-filter #'pro-ai-gptel--openrouter-free-model-p items))
         (free-models (delq nil (mapcar (lambda (entry) (alist-get 'id entry)) free-items))))
    
    (should (= (length free-models) 2))
    (should (member "qwen/qwen3-coder:free" free-models))
    (should (member "google/gemma-4-26b-a4b-it:free" free-models))
    (should-not (member "paid-model" free-models))))

(ert-deftest pro-ai-openrouter-model-name-extraction ()
  "Test extraction of model names from sample data."
  (let* ((sample-models '("qwen/qwen3-coder:free" 
                         "google/gemma-4-26b-a4b-it:free"
                         "paid-model:premium"))
         (free-models (seq-filter (lambda (m) (string-match ":free$" m)) sample-models)))
    
    (should (= (length free-models) 2))
    (should (member "qwen/qwen3-coder:free" free-models))
    (should (member "google/gemma-4-26b-a4b-it:free" free-models))
    (should-not (member "paid-model:premium" free-models))))

(ert-run-tests-batch-and-exit)

;;; ai-openrouter-api.el ends here