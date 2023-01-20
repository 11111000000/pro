;;; шрифты-для-огрызка.el --- Шрифты для MacOS
;;; Commentary:
;;; Code:

(defun update-font-settings () 
  (interactive) 
  (set-face-attribute 'default nil 
                      :family "Iosevka" 
                      :weight 'light 
                      :height 180) 
  (set-face-attribute 'fixed-pitch nil 
                      :family "Iosevka" 
                      :weight 'light 
                      :height 180) 
  (set-face-attribute 'font-lock-comment-face nil 
                      :family "Iosevka" 
                      :weight 'light 
                      :italic t 
                      :height 0.9))

(add-hook 'after-load-theme-hook 
          (lambda () 
            (update-font-settings)))

;; Межстрочный интервал

(update-font-settings)

(setq-default line-spacing 0.08)

;; Клавиши для управления шрифтом

;; MacOS style

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-set)

(provide 'шрифты-для-огрызка)
;;; шрифты-для-огрызка.el ends here
