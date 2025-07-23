;;; про-shell.el --- Поддержка терминала/оболочки -*- lexical-binding: t -*-
;;; Commentary:
;; seed для vterm и стандартных shell, активируется только при наличии bash/zsh.
;;; Code:

(when (or (executable-find "bash") (executable-find "zsh"))
  (use-package vterm
    :ensure t
    :commands (vterm))
  (defun про/open-vterm ()
    "Быстро открыть vterm."
    (interactive)
    (vterm))
  (global-set-key (kbd "C-c t") #'про/open-vterm)
  (message "Shell/vterm seed активирован."))

(provide 'про-shell)
;;; про-shell.el ends here
