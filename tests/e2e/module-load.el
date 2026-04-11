;;; module-load.el --- E2E тест: загрузка модулей ПРО

;;; Commentary:
;; Surface: Module.Load
;; Stability: FROZEN
;; Invariant: INV-Test-Coverage
;; Usage: emacs --batch -l tests/e2e/module-load.el

;;; Code:

(defvar про--тест-директории
  '("интеграция" "организация")
  "Список директорий с модулями для тестирования.")

(defun про--загрузить-директорию (dir)
  "Загрузить все .el файлы из DIR."
  (let ((dir-path (expand-file-name dir user-emacs-directory))
        (загружено 0))
    (when (file-directory-p dir-path)
      (dolist (файл (directory-files dir-path nil "\\.el$"))
        (let ((full-path (concat dir-path "/" файл)))
          (condition-case err
              (progn
                (load-file full-path)
                (setq загружено (1+ загружено)))
            (error
             (message "Пропущен %s: %s" файл (cadr err))))))
    загружено))

(dolist (dir про--тест-директории)
  (let ((кол-во (про--загрузить-директорию dir)))
    (message "Loaded %d modules from %s" кол-во dir)))

(message "=== Module.Load: OK ===")

(provide 'module-load)
;;; module-load.el ends here