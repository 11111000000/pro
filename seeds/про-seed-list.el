;;; про-seeds-list.el --- Автообзор seeds: список и комментарии -*- lexical-binding: t -*-
;;; Commentary:
;; Автоматически генерирует buфер со списком всех seeds/*.el с первой строкой docstring.
;;; Code:

(defun про/list-seeds ()
  "Показать список всех seeds/ с описанием."
  (interactive)
  (let* ((dir (expand-file-name "../seeds" user-emacs-directory))
         (files (sort (directory-files dir t "\\.el\\'") #'string<)))
    (with-current-buffer (get-buffer-create "*Seeds Index*")
      (erase-buffer)
      (insert (format "🌱 SEEDS (по Пути Дао)\n\n  Каталог: %s\n\n" dir))
      (dolist (file files)
        (let ((info (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (if (re-search-forward "^;;; \\(.*\\)--- \\(.*\\) -*-" nil t)
                          (match-string 2)
                        ""))))
          (insert (format "- %s — %s\n" (file-name-nondirectory file) info))))
      (goto-char (point-min))
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))

(provide 'про-seeds-list)
;;; про-seeds-list.el ends here
