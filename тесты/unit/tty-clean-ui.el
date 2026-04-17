;;; tty-clean-ui.el --- TTY clean UI regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Surface: TTY.CleanUI
;; Stability: FLUID
;; Usage: emacs --batch -l tests/unit/tty-clean-ui.el

;;; Code:

(require 'ert)
(require 'org)

(let ((root (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../../среда" root))
  (add-to-list 'load-path (expand-file-name "../../интеграция" root))
  (add-to-list 'load-path (expand-file-name "../../организация" root)))

(require 'про-текстовый-режим)
(require 'про-терминалы)

(ert-deftest pro-tty-clean-ui-setup-disables-org-prettification ()
  (with-temp-buffer
    (org-mode)
    (prettify-symbols-mode 1)
    (setq-local org-ellipsis "…"
                org-pretty-entities t)
    (про-tty-clean-ui-setup)
    (should (equal org-ellipsis "..."))
    (should-not org-pretty-entities)
    (should-not prettify-symbols-mode)))

(ert-deftest pro-tty-eshell-prompt-falls-back-to-text ()
  (let ((prompt (приглашение-eshell))
        (banner (pro/eshell-system-banner-string)))
    (should (string-match-p ">" prompt))
    (should-not (string-match-p "\|\|❯\|⎈" prompt))
    (should-not (string-match-p "👤\|⭐\|💻\|⏰" banner))))

(ert-run-tests-batch-and-exit)

;;; tty-clean-ui.el ends here
