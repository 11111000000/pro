;;; test-openrouter-models.el --- Audit OpenRouter free models -*- lexical-binding: t; -*-

;;; Code:

(require 'json)
(require 'url)
(require 'url-http)
(require 'subr-x)

(let ((core-file (expand-file-name "интеграция/про-ии-core.el" default-directory)))
  (load-file core-file))

(defun pro-ai-openrouter--request (model prompt)
  "Send one OpenRouter request for MODEL and PROMPT.
Return a plist with :ok, :status, :content and :raw."
  (let* ((api-key (про-ии--load-key-from-authinfo "openrouter.ai" "token"))
         (payload `((model . ,model)
                    (messages . [((role . "user")
                                  (content . ,prompt))])
                    (temperature . 0.2)
                    (max_tokens . 32)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " api-key))
            ("HTTP-Referer" . "https://github.com/zoya/pro")
            ("X-Title" . "pro-openrouter-audit")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously
               "https://openrouter.ai/api/v1/chat/completions" t t 30)))
    (if (not (bufferp buf))
        (list :ok nil :status 'timeout :content nil :raw nil)
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (let* ((status (if (re-search-forward "^HTTP/[0-9.]+ \([0-9]+\)" nil t)
                               (string-to-number (match-string 1))
                             nil))
                   (raw (buffer-substring-no-properties (point-min) (point-max))))
              (if (not (search-forward "\n\n" nil t))
                  (list :ok nil :status status :content nil :raw raw)
                (let ((json-object-type 'alist)
                      (json-array-type 'list)
                      (json-key-type 'symbol)
                      (json-false nil))
                  (condition-case err
                      (let* ((data (json-read))
                             (choices (alist-get 'choices data))
                             (message0 (and choices (alist-get 'message (car choices))))
                             (content (and message0 (alist-get 'content message0))))
                        (list :ok (and (integerp status) (= status 200) (stringp content) (> (length content) 0))
                              :status status
                              :content content
                              :raw raw))
                    (error
                     (list :ok nil :status status :content nil :raw raw :error err)))))))
        (kill-buffer buf)))))

(defun pro-ai-openrouter-audit-all-free-models ()
  "Audit every free OpenRouter model and print a report."
  (let* ((models (pro-ai-gptel-openrouter-free-models t))
         (total (length models))
         (results nil)
         (ok 0)
         (fail 0))
    (message "=== OpenRouter free models audit ===")
    (message "Free models found: %d" total)
    (dolist (model models)
      (message "Testing %s" model)
      (let* ((resp (pro-ai-openrouter--request model "Reply with one word: ok"))
             (status (plist-get resp :status))
             (content (plist-get resp :content))
             (success (plist-get resp :ok)))
        (push (list :model model :ok success :status status :content content :error (plist-get resp :error)) results)
        (if success
            (setq ok (1+ ok))
          (setq fail (1+ fail)))
        (message "  %s status=%s content=%s"
                 (if success "OK" "FAIL")
                 (or status "nil")
                 (if (and (stringp content) (> (length content) 0))
                     (substring content 0 (min 60 (length content)))
                   "<empty>"))))
    (setq results (nreverse results))
    (message "=== Summary ===")
    (message "Total: %d OK: %d FAIL: %d" total ok fail)
    (dolist (r results)
      (unless (plist-get r :ok)
        (message "FAIL %s status=%s error=%s content=%s"
                 (plist-get r :model)
                 (plist-get r :status)
                 (plist-get r :error)
                 (plist-get r :content))))
    results))

(pro-ai-openrouter-audit-all-free-models)

;;; test-openrouter-models.el ends here
