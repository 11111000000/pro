;;; про-keymap.el --- Лаконичные пользовательские биндинги (горячие клавиши) -*- lexical-binding: t -*-
;;; Commentary:
;; В этом seed сосредотачиваются только реально нужные глобальные сокращения:
;; быстрые функции вызова help, seed-обзора, очистки и запуска org-файлов.

;;; Code:

;; Запуск главного org-файла
(global-set-key (kbd "C-c o") (lambda () (interactive)
                               (find-file (expand-file-name "../org/main.org" user-emacs-directory))))

;; Обзор всех seeds и описание
(global-set-key (kbd "C-c s") #'про/list-seeds)

;; Чистка (уборка)
(global-set-key (kbd "C-c x") #'про/уборка)

;; Dao-режим (дзен)
(global-set-key (kbd "<f11>") #'про/zen-mode)

;; Перезапуск Emacs в чистом окружении (если seed присутствует)
(when (fboundp 'про/guix-restart)
  (global-set-key (kbd "C-c g") #'про/guix-restart))
(when (fboundp 'про/nix-restart)
  (global-set-key (kbd "C-c n") #'про/nix-restart))

;; Вывести краткую информацию о среде
(global-set-key (kbd "C-c e") #'про/print-env-info)

;; Быстрый доступ к истории команд/файлов (consult)
(when (featurep 'consult)
  (global-set-key (kbd "C-c r") #'consult-recent-file))

(provide 'про-keymap)
;;; про-keymap.el ends here
