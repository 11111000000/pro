;;; про-автодополнение.el --- Автодополнение Corfu + Cape -*- lexical-binding: t -*-
;;; Commentary:
;; Подключает Corfu + Cape + отключает ispell-capf для скорости.

;;; Code:

(use-package corfu
  :ensure t
  :init (global-corfu-mode 1)
  :custom ((corfu-auto t) (corfu-cycle t)))

(use-package cape
  :ensure t)

(defun disable-ispell-capf ()
  (setq-local completion-at-point-functions
              (remove #'ispell-completion-at-point completion-at-point-functions)))
(add-hook 'text-mode-hook #'disable-ispell-capf)
(add-hook 'prog-mode-hook #'disable-ispell-capf)

(provide 'про-автодополнение)
;;; про-автодополнение.el ends here
