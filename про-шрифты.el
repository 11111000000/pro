;;; шрифты.el --- Шрифты
;;; Commentary:
;;; Code:


(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  (help-mode . mixed-pitch-mode))

;;(setq-default default-frame-alist '((font . "Fira Code")))

;; function to convert from rubles to usd

(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  ;; (custom-set-faces '(default ((t (:family "DejaVu Sans Mono" :height 120)))))
  ;; (custom-set-faces '(fixed-pitch ((t (:family "DejaVu Sans Mono" :height 0.8)))))
  ;; (custom-set-faces '(variable-pitch ((t (:family "DejaVu Sans" :height 1.0)))))
  (custom-set-faces '(default ((t (:family "Source Code Pro" :height 120)))))
  (custom-set-faces '(fixed-pitch ((t (:family "Source Code Pro" :height 0.8)))))
  (custom-set-faces '(variable-pitch ((t (:family "Source Serif Pro" :height 1.0))))))

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook
          (lambda ()
              (обновить-настройки-шрифтов)))

(обновить-настройки-шрифтов)

;; Межстрочный интервал

(setq-default line-spacing 1)

(provide 'про-шрифты)
;;; про-шрифты.el ends here
