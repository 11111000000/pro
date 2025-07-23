;;; про-journal-auto.el --- Автосоздание дневника при первом запуске дня -*- lexical-binding: t -*-
;;; Commentary:
;; Запускает org-журнал утром/при первом запуске Emacs в новый день.
;;; Code:

(defvar про/last-journal-file
  (expand-file-name "../etc/last-journal-entry" user-emacs-directory)
  "Файл с датой последнего автосоздания дневника.")

(defun про/auto-open-journal ()
  (let* ((today (format-time-string "%Y-%m-%d"))
         (last (when (file-exists-p про/last-journal-file)
                 (with-temp-buffer
                   (insert-file-contents про/last-journal-file)
                   (buffer-string)))))
    (unless (equal today last)
      (run-at-time 2 nil #'про/open-today-journal)
      (with-temp-file про/last-journal-file (insert today)))))

(add-hook 'emacs-startup-hook #'про/auto-open-journal)

(provide 'про-journal-auto)
;;; про-journal-auto.el ends here
