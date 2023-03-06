;; -*- lexical-binding: t -*-
(require 'cl-lib)
(require 'consult)
(defun consult-completion-in-region (start end collection &optional predicate)
  (cl-letf* ((config (alist-get #'consult-completion-in-region consult--read-config))
             ;; Overwrite both the local and global value of `completion-styles', such that the
             ;; `completing-read' minibuffer sees the overwritten value in any case. This is
             ;; necessary if `completion-styles' is buffer-local.
             ;; NOTE: The completion-styles will be overwritten for recursive editing sessions!
             (cs (or (plist-get config :completion-styles) completion-styles))
             (completion-styles cs)
             ((default-value 'completion-styles) cs)
             (prompt (or (plist-get config :prompt) "Completion: "))
             (require-match (plist-get config :require-match))
             (preview-key (if (plist-member config :preview-key)
                              (plist-get config :preview-key)
                            consult-preview-key))
             (initial (buffer-substring-no-properties start end))
             (buffer (current-buffer))
             (metadata (completion-metadata initial collection predicate))
             (threshold (or (plist-get config :cycle-threshold) (completion--cycle-threshold metadata)))
             (all (completion-all-completions initial collection predicate (length initial))))
    ;; error if `threshold' is t or the improper list `all' is too short
    (if (and threshold
	     (or (not (consp (ignore-errors (nthcdr threshold all))))
		 (and completion-cycling completion-all-sorted-completions)))
        (completion--in-region start end collection predicate)
      (let* ((limit (car (completion-boundaries initial collection predicate "")))
             (category (completion-metadata-get metadata 'category))
             (exit-status 'finished)
             (completion
              (cond
               ((atom all) nil)
               ((and (consp all) (atom (cdr all)))
                (setq exit-status 'sole)
                (concat (substring initial 0 limit) (car all)))
               (t (car
                   (consult--with-preview
                       preview-key
                       ;; preview state
                       (consult--insertion-preview start end)
                       ;; transformation function
                       (if (eq category 'file)
                           (if (file-name-absolute-p initial)
                               (lambda (_inp cand) (substitute-in-file-name cand))
                             (lambda (_inp cand) (file-relative-name (substitute-in-file-name cand))))
                         (lambda (_inp cand) cand))
                       ;; candidate function
                       (apply-partially #'run-hook-with-args-until-success
                                        'consult--completion-candidate-hook)
                     (let ((enable-recursive-minibuffers t))
                       (if (eq category 'file)
                           ;; When completing files with consult-completion-in-region, the point in the
                           ;; minibuffer gets placed initially at the beginning of the last path component.
                           ;; By using the filename as DIR argument (second argument of read-file-name), it
                           ;; starts at the end of minibuffer contents, as for other types of completion.
                           ;; However this is undefined behavior since initial does not only contain the
                           ;; directory, but also the filename.
                           (read-file-name prompt initial initial require-match nil predicate)
                         (completing-read prompt
                                          (if (functionp collection)         ;; <<<<<
                                              (lambda (&rest args)           ;; <<<<<
                                                (with-current-buffer buffer  ;; <<<<<
                                                  (apply collection args)))  ;; <<<<<
                                            collection)                      ;; <<<<<
                                          predicate require-match initial)))))))))
        (if completion
            (progn
              (delete-region start end)
              (insert (substring-no-properties completion))
              (when-let (exit (plist-get completion-extra-properties :exit-function))
                (funcall exit completion exit-status))
              t)
          (message "No completion")
          nil)))))
