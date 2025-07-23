;;; про-auto-health.el --- Авто-ревизия состояния seeds и manifest -*- lexical-binding: t -*-
;;; Commentary:
;; При старте раз в неделю (или по требованию) выполняет health-check и напоминает сделать уборку.
;;; Code:

(defvar про/last-health-check-file
  (expand-file-name "../etc/last-health-check" user-emacs-directory)
  "Файл с датой последней диагностики.")

(defun про/auto-health-check ()
  "Автоматически проверить здоровье системы раз в 7 дней."
  (let* ((now (float-time (current-time)))
         (last (if (file-exists-p про/last-health-check-file)
                   (with-temp-buffer
                     (insert-file-contents про/last-health-check-file)
                     (string-to-number (buffer-string)))
                 0)))
    (when (> (/ (- now last) 86400) 7)
      (run-at-time 1 nil #'про/health-check)
      (with-temp-file про/last-health-check-file
        (insert (number-to-string now))))))

(add-hook 'emacs-startup-hook #'про/auto-health-check)

(provide 'про-auto-health)
;;; про-auto-health.el ends here
