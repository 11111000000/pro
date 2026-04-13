(require 'bytecomp)

(defvar про--syntax-ok nil)
(defvar про--syntax-fail nil)

(mapc
 (lambda (dir)
   (when (file-directory-p dir)
     (mapc
      (lambda (f)
        (let ((path (concat dir "/" f)))
          (condition-case err
              (progn
                (byte-compile-file path)
                (push (concat "OK: " path) про--syntax-ok))
            (error
             (push (format "FAIL: %s" (cadr err)) про--syntax-fail)))))
      (directory-files dir nil "\\.el$"))))
 '("интеграция" "организация"))

(message "=== L1 Syntax ===")
(message "OK: %d" (length про--syntax-ok))
(message "FAIL: %d" (length про--syntax-fail))
(mapc 'message про--syntax-fail)
(message "=== DONE ===")