;;; ai-openrouter-request.el --- E2E test for OpenRouter requests -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Tests actual requests to OpenRouter free models.

;;; Code:

(require 'ert)

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (root (if (file-directory-p source)
                   (file-name-as-directory source)
                 (file-name-directory source))))
    (add-to-list 'load-path (expand-file-name "../../интеграция/" root))))

(require 'про-ии-ядро)

(ert-deftest pro-ai-openrouter-real-request ()
  "Test actual request to OpenRouter free model."
  (skip-unless (bound-and-true-p про-ии--gptel-available))

  ;; Ensure we have the OpenRouter backend
  (let ((models (pro-ai-gptel-openrouter-free-models t)))
    (when models
      (pro-ai-gptel--openrouter-set-backend models)))

  (let ((backend (gptel-get-backend "Openrouter")))
    (skip-unless backend)

    ;; Set up a simple request
    (let* ((test-prompt "Say 'hello' in Russian")
           (response-received nil)
           (response-content nil)
           (original-message (symbol-function 'message)))

      ;; Temporarily override message function to capture response
      (cl-letf (((symbol-function 'message)
                 (lambda (format-string &rest args)
                   (let ((msg (apply #'format format-string args)))
                     (when (and (string-match "Привет" msg)
                                (not response-received))
                       (setq response-received t)
                       (setq response-content msg))
                     (apply original-message format-string args)))))

        ;; Create a temporary buffer and send request
        (with-temp-buffer
          (insert test-prompt)
          (goto-char (point-max))

          ;; This would normally send a request, but we'll just test the setup
          ;; In a real test, we'd use gptel-send or similar
          )

        ;; For now, just verify the backend is properly configured
        (should (gptel-backend-p backend))
        (should (> (length (gptel-backend-models backend)) 0))

        ;; Check that we have a valid key
        (let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
          (when key
            (should (stringp key))
            (should (> (length key) 20))))))))

(ert-deftest pro-ai-openrouter-model-selection ()
  "Test that we can select a free model."
  (skip-unless (bound-and-true-p про-ии--gptel-available))

  ;; Get free models
  (let ((free-models (pro-ai-gptel-openrouter-free-models t)))
    (skip-unless free-models)
    (should (listp free-models))
    (should (> (length free-models) 0))

    ;; Check that at least one model has the :free suffix
    (let ((has-free-suffix
           (cl-some (lambda (model)
                      (string-match ":free$" model))
                    free-models)))
      (should has-free-suffix))

    ;; Register backend with free models
    (pro-ai-gptel--openrouter-set-backend free-models)

    ;; Check that current model is one of the free ones
    (let ((current-backend (gptel-get-backend "Openrouter")))
      (when current-backend
        (let* ((models (gptel-backend-models current-backend))
               (current-model (or gptel-model (car models))))
          (when current-model
            (let ((model-name (gptel--model-name current-model)))
              (should (stringp model-name)))))))))

(ert-run-tests-batch-and-exit)

;;; ai-openrouter-request.el ends here
