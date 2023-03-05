;;; шрифты.el --- Шрифты
;;; Commentary:
;;; Code:

(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Fira Code"
                      :weight 'normal
                      :height 160)
  (set-face-attribute 'fixed-pitch nil
                      :family "Fira Code"
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :family "DejaVu Sans"
                      :weight 'normal))

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook
          (lambda ()
            (обновить-настройки-шрифтов)))

(обновить-настройки-шрифтов)

;; Межстрочный интервал

(setq-default line-spacing 1)

(provide 'шрифты)
;;; шрифты.el ends here
