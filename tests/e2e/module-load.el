;;; module-load.el --- E2E test: load modules PRO

(require 'bytecomp)

(defvar про--загружено 0)
(defvar про--ошибки 0)

(mapc
 (lambda (dir)
   (when (file-directory-p dir)
     (mapc
      (lambda (f)
        (let ((path (concat dir "/" f)))
          (condition-case err
              (progn
                (byte-compile-file path)
                (setq про--загружено (1+ про--загружено))
                (message "OK: %s" f))
            (error
             (setq про--ошибки (1+ про--ошибки))
             (message "SKIP: %s" (cadr err))))))
      (directory-files dir nil "\\.el$"))))
 '("интеграция" "организация"))

(message "=== Module.Load: %d OK, %d failed ===" про--загружено про--ошибки)

(provide 'module-load)
;;; module-load.el ends here