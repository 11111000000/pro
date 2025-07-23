;;; про-health-check.el --- Диагностика и гармония ПРО -*- lexical-binding: t -*-
;;; Commentary:
;; Проверка актуальности seeds, manifest, даёт советы по уборке и reproducibility.
;;; Code:

(defun про/health-check ()
  "Диагностировать состояние ПРО: чистота окружения, конфликтные seeds, несовпадения в manifest."
  (interactive)
  (with-current-buffer (get-buffer-create "*PRO: Dao Health*")
    (erase-buffer)
    (insert "🧘 ПРО-ДАО: Быстрая диагностика\n\n")

    ;; 1. Pure env
    (if (or (getenv "GUIX_ENVIRONMENT") (getenv "IN_NIX_SHELL"))
        (insert "✓ Чистое окружение (Guix или Nix detected).\n")
      (insert "! ВНИМАНИЕ: Вы НЕ в чистом Guix/Nix окружении.\n"))

    ;; 2. Старые seeds
    (let* ((dir (expand-file-name "../seeds" user-emacs-directory))
           (now (float-time))
           (old (seq-filter
                 (lambda (f)
                   (> (/ (- now (float-time (nth 5 (file-attributes f)))) 86400) 30))
                 (directory-files dir t "\\.el$"))))
      (if old
          (insert (format "! Старые seeds (>30д): %s\n"
                          (mapconcat #'file-name-nondirectory old ", ")))
        (insert "✓ Нет устаревших seeds (>30 дней).\n")))

    ;; 3. Manifest/use-package диагностика
    (if (fboundp 'про/compare-packages)
        (let ((msg (with-temp-message "" (про/compare-packages))))
          (insert (format "%s\n" (or msg ""))))
      (insert "… Не найден seed для сравнения manifest/use-package.\n"))

    (insert "\nПуть Дао: чистота, лаконизм, reproducibility.\nЗакрывайте buffer, если ясно.")
    (goto-char (point-min))
    (view-mode 1)
    (pop-to-buffer (current-buffer))))

;; Быстрый биндинг для диагностики
(global-set-key (kbd "C-c h") #'про/health-check)

(provide 'про-health-check)
;;; про-health-check.el ends here
