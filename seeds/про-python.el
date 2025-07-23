;;; про-python.el --- Минималистичный seed Python -*- lexical-binding: t -*-
;;; Commentary:
;; Активация поддержки Python только если python реально есть в manifest.
;;; Code:

(when (executable-find "python3")
  (use-package python
    :ensure nil
    :custom
    (python-shell-interpreter "python3"))
  (use-package blacken
    :ensure t
    :hook (python-mode . blacken-mode))
  (use-package pipenv
    :ensure t
    :hook (python-mode . pipenv-mode))
  (message "Python seed активирован."))

(provide 'про-python)
;;; про-python.el ends here
