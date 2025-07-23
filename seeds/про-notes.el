;;; про-notes.el --- Даосский seed для управления заметками -*- lexical-binding: t -*-
;;; Commentary:
;; Лёгкая система заметок с org-roam (если есть), или просто org. Автоматически не навязывает org-roam.
;;; Code:

(if (require 'org-roam nil 'noerror)
    (progn
      (setq org-roam-directory (expand-file-name "../org" user-emacs-directory))
      (use-package org-roam
        :ensure t
        :custom (org-roam-directory org-roam-directory)
        :init (org-roam-db-autosync-mode 1))
      (defun про/create-or-open-note ()
        "Создать или открыть org-roam заметку."
        (interactive)
        (call-interactively 'org-roam-node-find))
      (global-set-key (kbd "C-c n") #'про/create-or-open-note)
      (message "org-roam seed активирован."))
  ;; Без org-roam — просто быстрое создание org заметок.
  (defun про/create-simple-note ()
    "Быстро создать org заметку в org/notes/"
    (interactive)
    (let* ((dir (expand-file-name "../org/notes" user-emacs-directory))
           (file (expand-file-name (format-time-string "%Y%m%d-%H%M%S.org") dir)))
      (unless (file-directory-p dir) (make-directory dir t))
      (find-file file)
      (insert (format "#+TITLE: Заметка %s\n\n" (format-time-string "%Y-%m-%d %H:%M")))
      (message "Создана новая заметка: %s" file)))
  (global-set-key (kbd "C-c n") #'про/create-simple-note)
  (message "org-roam не обнаружен, работает простое org-замечание."))

(provide 'про-notes)
;;; про-notes.el ends here
