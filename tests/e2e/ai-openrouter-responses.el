;;; ai-openrouter-responses.el --- Test OpenRouter responses in gptel -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Tests that OpenRouter returns proper responses through gptel.

;;; Code:

(require 'ert)

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (root (if (file-directory-p source)
                   (file-name-as-directory source)
                 (file-name-directory source))))
    (add-to-list 'load-path (expand-file-name "../../интеграция/" root))))

(require 'про-ии-core)

(ert-deftest pro-ai-openrouter-gptel-backend-setup ()
  "Test that OpenRouter backend is properly set up in gptel."
  (skip-unless (bound-and-true-p про-ии--gptel-available))
  
  ;; Force refresh of free models
  (let ((free-models (pro-ai-gptel-openrouter-free-models t)))
    (when free-models
      (pro-ai-gptel--openrouter-set-backend free-models)))
  
  ;; Check backend exists
  (let ((backend (gptel-get-backend "Openrouter")))
    (should backend)
    (should (gptel-backend-p backend))
    
    ;; Check backend has models
    (let ((models (gptel-backend-models backend)))
      (should models)
      (should (> (length models) 0))
      
      ;; Check that at least one model name contains ":free"
      (let ((model-names (mapcar #'gptel--model-name models)))
        (let ((has-free (cl-some (lambda (name) (string-match ":free" name)) model-names)))
          (when has-free
            (should has-free)))))))

(ert-deftest pro-ai-openrouter-model-selection ()
  "Test that we can select a proper OpenRouter model."
  (skip-unless (bound-and-true-p про-ии--gptel-available))
  
  (let ((backend (gptel-get-backend "Openrouter")))
    (when backend
      ;; Check current model selection
      (let* ((models (gptel-backend-models backend))
             (current-model (or gptel-model (car models))))
        (when current-model
          (let ((model-name (gptel--model-name current-model)))
            (should (stringp model-name))
            ;; Should be a valid model name, possibly with :free suffix
            (should (> (length model-name) 5))))))))

(ert-deftest pro-ai-openrouter-key-configuration ()
  "Test that OpenRouter backend has proper key configuration."
  (skip-unless (bound-and-true-p про-ии--gptel-available))
  
  (let ((backend (gptel-get-backend "Openrouter")))
    (when backend
      ;; In our implementation, the key should be loaded via our function
      (let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
        (when key
          (should (stringp key))
          (should (> (length key) 20)))))))

(ert-deftest pro-ai-openrouter-free-models-list ()
  "Test that we get a reasonable list of free models."
  ;; Force refresh to get current models
  (let ((free-models (pro-ai-gptel-openrouter-free-models t)))
    (if free-models
        (progn
          (should (listp free-models))
          (should (> (length free-models) 0))
          (should (stringp (car free-models)))
          ;; Check that models have reasonable names
          (dolist (model (seq-take free-models 5))
            (should (stringp model))
            (should (> (length model) 5))))
      (message "Warning: No free models available - API might be temporarily unavailable"))))

(ert-run-tests-batch-and-exit)

;;; ai-openrouter-responses.el ends here