;;; про-поиск-пакетов.el --- Поиск несовпадающих пакетов в use-package vs manifest -*- lexical-binding: t -*-
;;; Commentary:
;; Диагностирует, все ли use-package Extensions имеются в manifest.scm или manifest.nix.
;; Выдаёт "лишние", "отсутствующие" — помогает поддерживать reproducibility на Пути Дао.
;;; Code:

(defun про/package-list-from-manifest-scm (path)
  "Return list of package names from manifest.scm by PATH."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (let ((lst '()))
        (goto-char (point-min))
        (while (re-search-forward "\"\\([a-z0-9A-Z-]+\\)\"" nil t)
          (push (match-string 1) lst))
        lst))))

(defun про/use-package-list ()
  "Return list of all explicitly use-package in loaded seeds (as symbols)."
  (let ((result '()))
    (dolist (file (directory-files (expand-file-name "../seeds" user-emacs-directory) t "\\.el$"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(use-package[ \t\n]+\\([^ :\n]+\\)" nil t)
          (push (intern (match-string 1)) result))))
    (delete-dups result)))

(defun про/compare-packages ()
  "Print diagnostic: вся ли seed'овая use-package-базис интегрирован в manifest."
  (interactive)
  (let* ((guix-pkg (про/package-list-from-manifest-scm (expand-file-name "../manifest.scm" user-emacs-directory)))
         (nix-pkg (про/package-list-from-manifest-scm (expand-file-name "../nix/manifest.nix" user-emacs-directory)))
         (manifest-pkg (append guix-pkg nix-pkg))
         (seed-pkg (mapcar (lambda (s) (symbol-name s)) (про/use-package-list)))
         (not-in-manifest (seq-difference seed-pkg manifest-pkg #'string=))
         (not-in-seeds (seq-difference manifest-pkg seed-pkg #'string=)))
    (message "В seeds присутствуют use-package без manifest: %s\nВ manifest, но не в seeds: %s"
             (or not-in-manifest "нет") (or not-in-seeds "нет"))))

(provide 'про-поиск-пакетов)
;;; про-поиск-пакетов.el ends here
