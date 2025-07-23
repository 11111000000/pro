;;; про-mail.el --- Seed: Почта (mu4e/offlineimap, минимально) -*- lexical-binding: t -*-
;;; Commentary:
;; Подключает mu4e, если установлен, настройки вручную в etc/mu4e.el.
;;; Code:

(when (require 'mu4e nil 'noerror)
  (setq mu4e-maildir (expand-file-name "~/Mail"))
  (setq mu4e-get-mail-command "offlineimap")
  (when (file-exists-p (expand-file-name "../etc/mu4e.el" user-emacs-directory))
    (load-file (expand-file-name "../etc/mu4e.el" user-emacs-directory)))
  (global-set-key (kbd "C-c m") #'mu4e)
  (message "mu4e seed (почта) активирован."))

(provide 'про-mail)
;;; про-mail.el ends here
