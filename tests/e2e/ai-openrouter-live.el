;;; ai-openrouter-live.el --- Audit OpenRouter free models -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)

(load-file (expand-file-name "../../интеграция/про-ии-core.el" (file-name-directory (or load-file-name buffer-file-name))))

(defun openrouter-audit--parse-body (body)
  (when (and (stringp body) (> (length body) 0))
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'list)
              (json-key-type 'symbol)
              (json-false nil))
          (json-read-from-string body))
      (error nil))))

(defun openrouter-audit--extract-content (json)
  (let* ((choices (alist-get 'choices json))
         (choice0 (car-safe choices))
         (msg (and choice0 (alist-get 'message choice0))))
    (and msg (alist-get 'content msg))))

(defun openrouter-audit--reason (status body json content)
  (cond
   ((null status) "no-status")
   ((= status 401) "unauthorized")
   ((= status 403) "forbidden")
   ((= status 404) "not-found")
   ((= status 429) "rate-limited")
   ((>= status 500) "server-error")
   ((null body) "empty-body")
   ((null json) "invalid-json")
   ((or (null content) (not (stringp content)) (string-empty-p content)) "empty-content")
   (t "ok")))

(defun openrouter-audit--request (model)
  (let* ((api-key (про-ии--load-key-from-authinfo "openrouter.ai" "token"))
         (payload `((model . ,model)
                    (messages . [((role . "user") (content . "Reply with one word: ok"))])
                    (temperature . 0.2)
                    (max_tokens . 16)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("Authorization" . ,(concat "Bearer " api-key))
            ("HTTP-Referer" . "https://github.com/zoya/pro")
            ("X-Title" . "pro-openrouter-audit")))
         (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
         (buf (url-retrieve-synchronously "https://openrouter.ai/api/v1/chat/completions" t t 30)))
    (if (not (bufferp buf))
        (list :model model :ok nil :status nil :reason "timeout" :content nil :body nil)
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (let* ((status url-http-response-status)
                   (body (when (search-forward "\n\n" nil t)
                           (buffer-substring-no-properties (point) (point-max))))
                   (json (openrouter-audit--parse-body body))
                   (content (openrouter-audit--extract-content json))
                   (reason (openrouter-audit--reason status body json content)))
              (list :model model
                    :ok (string= reason "ok")
                    :status status
                    :reason reason
                    :content content
                    :body body
                    :json json)))
        (kill-buffer buf)))))

(defun openrouter-audit--sleep-between (seconds)
  "Sleep SECONDS between requests."
  (when (and seconds (> seconds 0))
    (sleep-for seconds)))

(defcustom openrouter-audit-delay-seconds 2
  "Pause between OpenRouter requests during audit."
  :type 'number
  :group 'applications)

(defcustom openrouter-audit-max-models nil
  "Limit number of OpenRouter models to test. nil means all."
  :type '(choice (const :tag "All" nil) integer)
  :group 'applications)

(defun openrouter-audit--models-to-test ()
  (let* ((models (pro-ai-gptel-openrouter-free-models t))
         (limit openrouter-audit-max-models))
    (if (and (integerp limit) (> limit 0))
        (seq-take models limit)
      models)))

(defun openrouter-audit-run ()
  (let* ((models (pro-ai-gptel-openrouter-free-models t))
         (results nil)
         (ok 0)
         (fail 0))
    (message "=== OpenRouter free models audit ===")
    (message "Free models found: %d" (length models))
    (dolist (model (openrouter-audit--models-to-test))
      (message "Testing %s" model)
      (let ((r (openrouter-audit--request model)))
        (push r results)
        (if (plist-get r :ok)
            (setq ok (1+ ok))
          (setq fail (1+ fail)))
        (message "  %s status=%s reason=%s content=%s"
                 (if (plist-get r :ok) "OK" "FAIL")
                 (or (plist-get r :status) "nil")
                 (plist-get r :reason)
                 (let ((c (plist-get r :content)))
                   (if (and (stringp c) (> (length c) 0))
                       (substring c 0 (min 60 (length c)))
                     "<empty>")))))
      (openrouter-audit--sleep-between openrouter-audit-delay-seconds))
    (setq results (nreverse results))
    (message "=== Summary ===")
    (message "Total: %d OK: %d FAIL: %d" (+ ok fail) ok fail)
    (dolist (r results)
      (unless (plist-get r :ok)
        (message "FAIL %s status=%s reason=%s content=%s"
                 (plist-get r :model)
                 (or (plist-get r :status) "nil")
                 (plist-get r :reason)
                 (let ((c (plist-get r :content)))
                   (if (and (stringp c) (> (length c) 0))
                       (substring c 0 (min 80 (length c)))
                     "<empty>")))))
    (list :ok ok :fail fail :results results))

(defun openrouter-audit-main ()
  (let ((summary (openrouter-audit-run)))
    (kill-emacs (if (> (plist-get summary :ok) 0) 0 1))))

(openrouter-audit-main)

;;; ai-openrouter-live.el ends here
