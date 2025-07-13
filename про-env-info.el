;;; про-env-info.el --- Интерактивная информация об окружении ПРО -*- lexical-binding: t -*-
;;; Commentary:
;; Показывает в минибуфере сведения о платформе (Guix, Nix, чистый shell, используемые ядра).
;; Для гармонии: никакого давления, просто вывод сведений по команде.
;;; Code:

(defun про/print-env-info ()
  "Показать текущие сведения о среде воспроизводимости (Guix/Nix/pure shell)."
  (interactive)
  (message
   (concat
    "PRO ДАО среда:\n"
    " — Guix: " (if (getenv "GUIX_ENVIRONMENT") "да" "нет") "\n"
    " — Nix: "  (if (getenv "IN_NIX_SHELL") "да" "нет") "\n"
    " — Emacs: " emacs-version "\n"
    " — OS: " (or (getenv "XDG_CURRENT_DESKTOP") system-type) "\n"
    " — user-emacs-directory: " (expand-file-name user-emacs-directory)
    )))

(provide 'про-env-info)
;;; про-env-info.el ends here
