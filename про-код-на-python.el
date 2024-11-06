;;; про-код-на-python.el --- PYTHON -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;; Python mode

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
       (list (lambda ()
               (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
       (list (lambda ()
               (setq python-shell-interpreter "python3")))))

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))

(provide 'про-код-на-python)
;;; про-код-на-python.el ends here

