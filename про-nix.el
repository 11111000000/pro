;;; про-nix.el --- Поддержка Nix -*- lexical-binding: t -*-
;; Nix
;;; Commentary:
;;; Code:

(when (file-exists-p "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh")
     (setenv "NIX_PROFILES"
             (concat "/nix/var/nix/profiles/per-user/"
                     (user-login-name)
                     "/profile:/nix/var/nix/profiles/default"))
     (setenv "NIX_PATH" "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs")
     (let ((profile-script "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"))
       (when (file-exists-p profile-script)
         (shell-command (concat ". " profile-script " && env") "*Nix-env/")
         (with-current-buffer "*Nix-env/"
           (goto-char (point-min))
           (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
             (setenv (match-string 1) (match-string 2))))
         (kill-buffer "*Nix-env/"))))


(setenv "PATH"
        (concat (expand-file-name "~/.nix-profile/bin:")
                (getenv "PATH")))
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))

(provide 'про-nix)
;;; про-nix.el ends here
