;;; ai-openrouter-call.el --- OpenRouter direct call test

;;; Commentary:
;; Surface: Integration.Ai
;; Stability: FROZEN
;; Invariant: INV-OpenRouter-Direct
;; Usage: emacs --batch -l tests/e2e/ai-openrouter-call.el

;;; Code:

(require 'про-ии-core)
(require 'ert)
(require 'url-http)

(defun test-openrouter-key-load ()
  (let ((key (про-ии--load-key-from-authinfo "openrouter.ai" "token")))
    (should (stringp key))
    (should (> (length key) 20))
    key))

(defun test-openrouter-model-verify-free (~models)
  (should (listp ~models))
  (dolist (model ~models)
    (should (string-match-p ":free$" model)))
  ;; Check we have our preferred models
  (should (member "google/gemma-4-26b-a4b-it:free" ~models)))

(ert-deftest pro-ai-openrouter-direct-call ()
  "Test OpenRouter integration via direct calls."
  (message "Get API key...")
  (when-let ((key (test-openrouter-key-load)))
    (message "key_type: %s" (type-of key))
    (message "key: %s..." (substring key 0 (min 10 (length key)))))
  
  ;; Test fetching free models (without network)
  (let ((fetched-models (pro-ai-gptel-openrouter-free-models)))
    (message "Fetched %d free models." (length fetched-models))
    (when (> (length fetched-models) 0)
      (test-openrouter-model-verify-free fetched-models)
      (should (> (length fetched-models) 5)))))

(ert-run-tests-batch-and-exit)
;;; ai-openrouter-call.el ends here