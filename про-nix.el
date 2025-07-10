;;; про-nix.el --- Поддержка Nix -*- lexical-binding: t -*-
;; Nix
;;; Commentary:
;;; Code:

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
  ;:hook (nix-mode . lisp-interaction-mode) ;; если нужен дополнительный режим
  :config
  ;; Можно добавить дополнительные настройки тут.
  )


;; (when (file-exists-p "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh")
;;      (setenv "NIX_PROFILES"
;;              (concat "/nix/var/nix/profiles/per-user/"
;;                      (user-login-name)
;;                      "/profile:/nix/var/nix/profiles/default"))
;;      (setenv "NIX_PATH" "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs")
;;      (let ((profile-script "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"))
;;        (when (file-exists-p profile-script)
;;          (shell-command (concat ". " profile-script " && env") "*Nix-env/")
;;          (with-current-buffer "*Nix-env/"
;;            (goto-char (point-min))
;;            (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
;;              (setenv (match-string 1) (match-string 2))))
;;          (kill-buffer "*Nix-env/"))))


;; (setenv "PATH"
;;         (concat (expand-file-name "~/.nix-profile/bin:")
;;                 (getenv "PATH")))
;; (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))

(defun про/пересобрать-nix ()
  "Запустить 'sudo nixos-rebuild switch' с конфигом пользователя и показать вывод в буфере *nixos-log*."
  (interactive)
  (let ((cmd (format "sudo nixos-rebuild switch -I nixos-config=%s/.config/nixos/configuration.nix"
                     (getenv "HOME")))
        (log-buf "*nixos-log*"))
    (with-current-buffer (get-buffer-create log-buf)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "Выполняем: %s\n\n" cmd))
      (read-only-mode 1))
    (let ((proc (start-process-shell-command "nixos-rebuild" log-buf cmd)))
      (set-process-sentinel
       proc
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (with-current-buffer (process-buffer proc)
             (read-only-mode 0)
             (goto-char (point-max))
             (insert (format "\n--- Завершено с кодом %d ---\n" (process-exit-status proc)))
             (read-only-mode 1))
           (display-buffer (process-buffer proc))))))
    (display-buffer log-buf)))

(provide 'про-nix)
;;; про-nix.el ends here
