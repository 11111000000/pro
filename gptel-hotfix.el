;;; gptel-hotfix.el --- Temporary workaround for gptel stream cleanup search errors -*- lexical-binding: t; -*-

;; This advice prevents rare "Search failed: <hash>" errors in gptel process sentinel
;; by safely handling missing cleanup markers. If the Curl -w token is absent,
;; we attempt a fallback parse: recover HTTP status and JSON body directly
;; from the buffer and finalize the FSM cleanly.

(defvar atlas-gptel-hotfix-dump t
  "When non-nil, write the process buffer to a temp file when the Curl token is missing.")
(defvar atlas-gptel-hotfix-dump-dir nil
  "Directory for writing fallback dumps. If nil, use =temporary-file-directory'.")

(with-eval-after-load 'gptel-request
  (defun atlas--gptel-safe-cleanup (orig-fn &rest args)
    "Wrap `gptel-curl--stream-cleanup' and gracefully finalize on missing token.

If `search-backward' fails while locating the Curl token, attempt to
recover the HTTP status and parse the body without the token. Always
finalize the FSM and invoke the callback to avoid a stuck state."
    (condition-case err
        (apply orig-fn args)
      (search-failed
       (let* ((process (car args))
              (proc-buf (and (processp process) (process-buffer process)))
              (fsm (and (processp process)
                        (car (alist-get process gptel--request-alist)))))
         (message "gptel: stream cleanup could not locate token (%s); using fallback" (cadr err))
         (let* ((exit (and (processp process) (process-exit-status process)))
                (pstat (and (processp process) (process-status process)))
                (cmd (and (processp process) (mapconcat #'identity (process-command process) " "))))
           (message "gptel: fallback diag: proc-status=%S exit=%S" pstat exit)
           (when cmd (message "gptel: curl cmd: %s" cmd)))
         (when (and (numberp exit) (= exit 126))
           (with-current-buffer proc-buf
             (save-excursion
               (goto-char (point-min))
               (when (search-forward "Argument list too long" nil t)
                 (message "gptel: Curl failed with 'Argument list too long'. Set gptel-curl-file-size-threshold to 0 (use temp file) or shorten the prompt.")))))
         (when (and atlas-gptel-hotfix-dump (buffer-live-p proc-buf))
           (let* ((dir (or atlas-gptel-hotfix-dump-dir temporary-file-directory))
                  (file (expand-file-name (format "gptel-fallback-%s.log"
                                                  (format-time-string "%Y%m%d-%H%M%S"))
                                          dir)))
             (with-current-buffer proc-buf
               (write-region (point-min) (point-max) file nil 'quiet))
             (message "gptel: fallback dump written to %s" file)))
         (when (and fsm (buffer-live-p proc-buf))
           (let* ((info (gptel-fsm-info fsm))
                  (cb (plist-get info :callback))
                  http-status http-msg header-end response error-data)
             ;; Try to log whatever we can without the token
             (when gptel-log-level
               (ignore-errors
                 (with-current-buffer proc-buf
                   (save-excursion
                     (goto-char (point-min))
                     (when (re-search-forward "\r?\n\r?\n" nil t)
                       (let ((p (point)))
                         (gptel--log (gptel--json-encode (buffer-substring-no-properties (point-min) (1- p)))
                                     "response headers")
                         (gptel--log (buffer-substring-no-properties p (point-max))
                                     "response body")))))))
             ;; Fallback: recover HTTP status and header/body split
             (with-current-buffer proc-buf
               (save-excursion
                 (goto-char (point-min))
                 (while (re-search-forward "^HTTP/[.0-9]+ +\\([0-9]+\\)\\(.*\\)$" nil t)
                   (setq http-status (match-string 1))
                   (setq http-msg (string-trim (buffer-substring (line-beginning-position) (line-end-position)))))
                 (let (sep)
                   (goto-char (point-min))
                   (while (re-search-forward "\r?\n\r?\n" nil t)
                     (setq sep (match-end 0)))
                   (setq header-end sep))
                 ;; Parse JSON body if this looks like an error
                 (when (and header-end (not (member http-status '("200" "100"))))
                   (goto-char header-end)
                   (setq response (condition-case nil
                                      (gptel--json-read)
                                    (error 'json-read-error)))
                   (setq error-data
                         (cond ((plistp response) (plist-get response :error))
                               ((arrayp response)
                                (cl-some (lambda (el) (plist-get el :error)) response)))))))
             ;; Store recovered status in info
             (when http-status
               (plist-put info :http-status http-status)
               (message "gptel: fallback parsed HTTP status %s%s"
                        http-status
                        (if http-msg (format " (%s)" http-msg) "")))
             (when http-msg    (plist-put info :status http-msg))
             ;; Decide success/failure and invoke callback
             (cond
              ((member http-status '("200" "100"))
               (with-demoted-errors "gptel callback error: %S"
                 (funcall cb t info)))
              (t
               (cond
                (error-data
                 (plist-put info :error error-data))
                ((eq response 'json-read-error)
                 (plist-put info :error "Malformed JSON in response."))
                (t
                 (plist-put info :error "Could not parse HTTP response (no Curl token).")))
               (with-demoted-errors "gptel callback error: %S"
                 (funcall cb nil info))))
             ;; Move FSM forward and clean up
             (gptel--fsm-transition fsm))
           (setf (alist-get process gptel--request-alist nil 'remove) nil))
         (when (buffer-live-p proc-buf)
           (kill-buffer proc-buf))))
      (error
       (message "gptel: cleanup error (suppressed): %S" err))))
  (advice-add 'gptel-curl--stream-cleanup :around #'atlas--gptel-safe-cleanup))

(provide 'gptel-hotfix)
;;; gptel-hotfix.el ends here
