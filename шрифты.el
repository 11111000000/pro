;;; шрифты.el --- Шрифты
;;; Commentary:
;;; Code:

(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  (set-face-attribute 'default nil
                      :family "Iosevka"
                      :weight 'normal
                      :height 120)
  (set-face-attribute 'fixed-pitch nil
                      :family "Iosevka"
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :family "Iosevka Aile"
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
