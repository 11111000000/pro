;;; про-journal.el --- Seed: Ведение дневника и журналов в духе Дао -*- lexical-binding: t -*-
;;; Commentary:
;; Автоматическое создание ежедневного org-журнала в org/journal/;
;; минималистичная интеграция — простое расширение для фиксации жизни.
;;; Code:

(defvar про/journal-directory
  (expand-file-name "../org/journal/" user-emacs-directory)
  "Путь к директории дневниковых файлов.")

(defun про/open-today-journal ()
  "Открыть или создать ежедневный org-журнал."
  (interactive)
  (let* ((dir про/journal-directory)
         (file (expand-file-name
                (format-time-string "%Y-%m-%d.org") dir)))
    (unless (file-directory-p dir) (make-directory dir t))
    (find-file file)
    (unless (> (buffer-size) 0)
      (insert (format "#+TITLE: Журнал %s\n\n* %s\n\n"
                      (format-time-string "%Y-%m-%d")
                      (format-time-string "%H:%M %A")))
      (save-buffer))
    (goto-char (point-max))
    (message "Журнал на сегодня готов.")))

(global-set-key (kbd "C-c j") #'про/open-today-journal)

(message "Journal seed (дневник) активирован.")

(provide 'про-journal)
;;; про-journal.el ends here
