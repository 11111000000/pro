;;; package --- Summary
;; Шрифты для MacOS
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
                      :height 0.9)
  ;; (set-face-attribute 'default nil :family "Monaco" :weight 'regular :height 120)
  ;; (set-face-attribute 'fixed-pitch nil :family "Monaco" :weight 'regular :height 120)
  ;; (set-face-attribute 'font-lock-comment-face nil :family "Menlo" :weight 'light :italic t)
  ;; (set-face-attribute 'org-level-1 nil :family "Liberation Serif" :height 180)
  ;; (set-face-attribute 'outshine-level-1 nil :family "Liberation Serif" :height 180)
  )

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
