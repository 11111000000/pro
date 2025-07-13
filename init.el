;;; init.el --- Минимальный стартовый init ПРО -*- lexical-binding: t -*-
(add-to-list 'load-path ".")
(require 'про-менеджер-пакетов)
(require 'про-внешний-вид)
(require 'про-шрифты)
(require 'про-код)
(require 'про-графическую-среду)
(require 'про-env-info)

;; ------------------------------------------------------------
;;  Автозагрузка «семян» (seeds/) ─ необязательных расширений.
;;  Всё, что помещено в каталог  ../seeds/*.el  (относительно
;;  user-emacs-directory) будет загружено автоматически, если
;;  файл заканчивается на “.el”.  Ошибки при загрузке семян
;;  не прерывают работу основного init.
;; ------------------------------------------------------------
(let* ((seeds-dir (expand-file-name "../seeds" user-emacs-directory)))
  (when (file-directory-p seeds-dir)
    (dolist (f (directory-files seeds-dir t "\\.el\\'"))
      (ignore-errors (load-file f)))))

;;; init.el ends here
