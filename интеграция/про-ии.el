;;; про-ии.el --- Совместимый загрузчик AI-интеграции -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Лёгкая точка входа для AI-интеграции ПРО.
;; Загружает только ядро GPTEL и интерактивные команды.

;;; Code:

(eval-and-compile
  (let* ((source (or load-file-name buffer-file-name default-directory))
         (dir (if (file-directory-p source)
                  (file-name-as-directory source)
                (file-name-directory source)))
         (infra (expand-file-name "../инфраструктура/" dir)))
    (add-to-list 'load-path (file-name-as-directory dir))
    (when (file-directory-p infra)
      (add-to-list 'load-path (file-name-as-directory infra)))))

(require 'про-ии-core)
(require 'про-ии-команды)

(provide 'про-ии)
;;; про-ии.el ends here
