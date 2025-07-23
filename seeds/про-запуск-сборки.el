;;; про-запуск-сборки.el --- Быстрый запуск сборки окружения и Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; Seed-команда для вызова "guix shell" или "nix develop" прямо из Emacs.
;; Полезно при миграции или запуске нового окна с чистым окружением.
;;; Code:

(defun про/guix-restart ()
  "Перезапустить Emacs в чистом окружении Guix shell."
  (interactive)
  (let ((cmd (format "guix shell -m %s/manifest.scm -- emacs --init-directory %s/.emacs.d &"
                     (file-name-directory user-emacs-directory)
                     (file-name-directory user-emacs-directory))))
    (shell-command cmd)
    (message "Запущен новый экземпляр Emacs через Guix shell.")))

(defun про/nix-restart ()
  "Перезапустить Emacs в чистом Nix shell."
  (interactive)
  (let ((cmd (format "nix develop %s/nix/manifest.nix -c emacs --init-directory %s/.emacs.d &"
                     (file-name-directory user-emacs-directory)
                     (file-name-directory user-emacs-directory))))
    (shell-command cmd)
    (message "Запущен новый экземпляр Emacs через Nix shell.")))

(provide 'про-запуск-сборки)
;;; про-запуск-сборки.el ends here
