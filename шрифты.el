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
                      :weight 'normal)
  ;; (set-face-attribute 'font-lock-comment-face nil
  ;;                     :family "Fira Sans"
  ;;                     :weight 'normal
  ;;                     :italic t)
  ;; (set-face-attribute 'outline-1 nil
  ;;                     :family "Fira Sans"
  ;;                     :height 1.3
  ;;                     :weight 'light
  ;;                     :italic nil)
  ;;   (set-face-attribute 'outline-2 nil
  ;;                     :family "Fira Sans"
  ;;                     :height 1.15
  ;;                     :weight 'light
  ;;                     :italic nil)
  ;; (set-face-attribute 'org-level-1 nil :family "Liberation Serif" :height 180)
  ;; (set-face-attribute 'outshine-level-1 nil :family "Liberation Serif" :height 180)
  )

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook
          (lambda ()
            (обновить-настройки-шрифтов)))

(обновить-настройки-шрифтов)

;; Межстрочный интервал
(setq-default line-spacing 1)

(provide 'шрифты)
;;; шрифты.el ends here
