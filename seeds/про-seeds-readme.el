;;; про-seeds-readme.el --- Автогенерация описания всех seeds в README-seeds.org -*- lexical-binding: t -*-
;;; Commentary:
;; Генерирует catalog всех seeds/ c первой строкой docstring + commentary прямо в файл.
;;; Code:

(defun про/generate-seeds-readme ()
  "Создать org-файл README-seeds.org с описаниями всех seeds."
  (interactive)
  (let* ((seeds-dir (expand-file-name "../seeds" user-emacs-directory))
         (out (expand-file-name "../README-seeds.org" user-emacs-directory))
         (files (sort (directory-files seeds-dir t "\\.el\\'") #'string<)))
    (with-temp-file out
      (insert "#+TITLE: Seeds: Семена расширений ПРО\n"
              "#+AUTHOR: Система Дао (автоматически)\n"
              "\n* Seeds\n\n")
      (dolist (file files)
        (let ((name (file-name-nondirectory file))
              (docstr "")
              (comment nil))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward "^;;; .*--- \\(.*\\) [-*]" nil t)
              (setq docstr (match-string 1)))
            (when (re-search-forward "^;;; Commentary:\\(?:\n\\| \\)+\\(.*\\(?:\n;;.*\\)*\\)" nil t)
              (setq comment
                    (replace-regexp-in-string
                     "^;; *" ""
                     (match-string 1)))))
          (insert (format "** %s\n    %s\n%s\n" name docstr (or comment ""))))))
    (message "README-seeds.org обновлён.")))

(provide 'про-seeds-readme)
;;; про-seeds-readme.el ends here
