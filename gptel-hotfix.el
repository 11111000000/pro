;;; gptel-hotfix.el --- Temporary workaround for gptel stream cleanup search errors -*- lexical-binding: t; -*-

;; This advice prevents rare "Search failed: <hash>" errors in gptel process sentinel
;; by safely handling missing cleanup markers. If the Curl -w token is absent,
;; we attempt a fallback parse: recover HTTP status and JSON body directly
;; from the buffer and finalize the FSM cleanly.

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
             (when http-status (plist-put info :http-status http-status))
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
