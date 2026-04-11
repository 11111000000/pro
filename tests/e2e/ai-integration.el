;;; ai-integration.el --- E2E тест: ИИ-интеграция ПРО

;;; Commentary:
;; Surface: Integration.Ai
;; Stability: FROZEN
;; Invariant: INV-Test-Coverage
;; Usage: emacs --batch -l tests/e2e/ai-integration.el

;;; Code:

(add-to-list 'load-path (expand-file-name "интеграция" user-emacs-directory))

(require 'про-ии)

(message "=== AI integration loaded ===")

(provide 'ai-integration)
;;; ai-integration.el ends here