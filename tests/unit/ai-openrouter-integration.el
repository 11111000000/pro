;;; ai-openrouter-integration.el --- Integration tests for OpenRouter -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Tests OpenRouter integration including key loading and model fetching.

;;; Code:

(require 'ert)

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (root (if (file-directory-p source)
                   (file-name-as-directory source)
                 (file-name-directory source))))
    (add-to-list 'load-path (expand-file-name "../../интеграция/" root))))

(require 'про-ии-core)

(ert-deftest pro-ai-openrouter-key-loading ()
  "Test that OpenRouter key can be loaded from authinfo."
  (let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
    (if key
        (should (stringp key))
      (message "Warning: No OpenRouter key found in ~/.authinfo - test skipped"))))

(ert-deftest pro-ai-openrouter-model-fetching ()
  "Test that OpenRouter models can be fetched."
  (let ((models (pro-ai-gptel-openrouter-free-models t)))
    (if models
        (progn
          (should (listp models))
          (should (> (length models) 0))
          (should (stringp (car models))))
      (message "Warning: Could not fetch OpenRouter models - API may be unavailable"))))

(ert-deftest pro-ai-openrouter-backend-registration ()
  "Test that OpenRouter backend can be registered."
  (when (bound-and-true-p про-ии--gptel-available)
    (let ((models (pro-ai-gptel-openrouter-free-models t)))
      (if models
          (progn
            (pro-ai-gptel--openrouter-set-backend models)
            (let ((backend (gptel-get-backend "Openrouter")))
              (if backend
                  (should (gptel-backend-p backend))
                (message "Warning: Could not register OpenRouter backend"))))
        (message "Warning: No models available for backend registration")))))

(ert-run-tests-batch-and-exit)

;;; ai-openrouter-integration.el ends here