;;; менеджер-пакетов-straight.el --- Пакетный менеджер Straight
;;; Commentary:
;;; Code:

(setq straight-use-package-by-default t) ;; have leaf use straight.el by default.

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(straight-use-package 'org)

(provide 'менеджер-пакетов-straight)
;;; менеджер-пакетов-straight.el ends here
