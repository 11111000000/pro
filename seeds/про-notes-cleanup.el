;;; про-notes-cleanup.el --- Seed для ревизии заметок и журналов -*- lexical-binding: t -*-
;;; Commentary:
;; Автоматическая уборка org/notes/ и org/journal/ старше 180 дней (по запросу).
;;; Code:

(defun про/clean-old-notes (&optional days)
  "Удалить/org/notes и org/journal файлы старше DAYS (по умолчанию 180)."
  (interactive "nУдалить заметки старше (дней, по умолчанию 180): ")
  (let ((days (or days 180)))
    (dolist (dir (list (expand-file-name "../org/notes" user-emacs-directory)
                       (expand-file-name "../org/journal" user-emacs-directory)))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.org\\'"))
          (let* ((age (/ (- (float-time) (float-time (nth 5 (file-attributes file)))) 86400)))
            (when (> age days)
              (message "Удаляем (устарело %s дней): %s" (truncate age) (file-name-nondirectory file))
              (delete-file file))))))))

(provide 'про-notes-cleanup)
;;; про-notes-cleanup.el ends here
