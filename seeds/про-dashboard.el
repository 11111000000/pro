;;; про-dashboard.el --- Пустой приветственный буфер Дао -*- lexical-binding: t -*-
;;; Commentary:
;; Простая реализация стартового экрана с даосской цитатой — не dashboard в духе нагруженности, а лёгкое приветствие.
;; Отключаемый seed: просто уберите этот файл или переименуйте, чтобы вернуть абсолютную пустоту.
;;; Code:

(defun про/show-dao-dashboard ()
  "Показать даосский стартовый экран в *scratch*."
  (with-current-buffer "*scratch*"
    (setq-local inhibit-read-only t)
    (erase-buffer)
    (insert "\n   大道废，有仁义\n   *Когда Дао забыли, появились мораль и долг.*\n\n   — Добро пожаловать!\n\n   (Удалите seeds/про-dashboard.el для ещё большей пустоты)\n\n")
    (setq-local inhibit-read-only nil)
    (goto-char (point-min))))
  
(add-hook 'emacs-startup-hook #'про/show-dao-dashboard)

(provide 'про-dashboard)
;;; про-dashboard.el ends here
