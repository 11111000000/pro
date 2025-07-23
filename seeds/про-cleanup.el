;;; про-cleanup.el --- Seed для регулярной уборки рабочей среды -*- lexical-binding: t -*-
;;; Commentary:
;; Автоматическая уборка устаревших файлов в seeds/ и undo/, а также очистка истории recentf.
;; Вызывайте вручную: M-x про/уборка, или назначьте на таймер.
;;; Code:

(defun про/clean-seeds (days)
  "Удалить из seeds/ все файлы старше DAYS."
  (interactive "nУдалить в seeds/ файлы старше (дней): ")
  (let* ((dir (expand-file-name "../seeds" user-emacs-directory))
         (now (float-time)))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (when (> (/ (- now (float-time (nth 5 (file-attributes file)))) 86400) days)
        (message "Удаляем старое семя: %s" (file-name-nondirectory file))
        (delete-file file)))))

(defun про/clean-undo ()
  "Очистить папку undo/ в user-emacs-directory."
  (interactive)
  (let ((dir (expand-file-name "undo" user-emacs-directory)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir t "^[^.].*"))
        (ignore-errors (delete-file file)))
      (message "Undo history очищено!"))))

(defun про/clean-recentf ()
  "Очистить историю recentf."
  (interactive)
  (when (bound-and-true-p recentf-mode)
    (setq recentf-list nil)
    (recentf-save-list)
    (message "recentf очищен.")))

(defun про/уборка ()
  "Выполнить все процедуры по уборке: seeds, undo, recentf."
  (interactive)
  (call-interactively #'про/clean-seeds)
  (про/clean-undo)
  (про/clean-recentf)
  (message "Пусть пустота воцарится во всех уголках!"))

(provide 'про-cleanup)
;;; про-cleanup.el ends here
