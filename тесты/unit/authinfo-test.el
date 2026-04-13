(require 'auth-source)

(defun test-load-key ()
  (let ((key (auth-source-pick-first-password :host "openrouter.ai" :user "openrouter")))
    (message "Loaded key for openrouter: %s" key)))

(test-load-key)

;; End of test