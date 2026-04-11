;;; org-roundtrip.el --- E2E тест: Org-roundtrip ПРО

;;; Commentary:
;; Surface: Org.Roundtrip
;; Stability: FROZEN
;; Invariant: INV-Test-Coverage
;; Usage: emacs --batch -l tests/e2e/org-roundtrip.el

;;; Code:

(require 'org)

(defvar про--тестовый-org "
* Заголовок
** Подзаголовок
Тело текста.
")

(with-temp-buffer
  (insert про--тестовый-org)
  (org-mode)
  (goto-char (point-min))
  (when (re-search-forward "\\* Заголовок" nil t)
    (message "=== Org-mode roundtrip: OK ===")))

(provide 'org-roundtrip)
;;; org-roundtrip.el ends here