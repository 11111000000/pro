;;; healthcheck.el --- E2E тест: проверка работоспособности ПРО

;;; Commentary:
;; Surface: Healthcheck
;; Stability: FROZEN
;; Invariant: INV-Test-Coverage
;; Usage: emacs --batch -l tests/e2e/healthcheck.el

;;; Code:

(add-to-list 'load-path (expand-file-name "интеграция" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "организация" user-emacs-directory))

(require 'про-ии)
(require 'про-организацию)

(message "=== Healthcheck: OK ===")

(provide 'healthcheck)
;;; healthcheck.el ends here