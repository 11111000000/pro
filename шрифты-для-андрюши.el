;;;; Шрифт

(defun update-font-settings () 
  (interactive) 
  (set-face-attribute 'default nil 
                      :family "Droid Sans Mono" 
                      :weight 'normal 
                      :height 100) 
  (set-face-attribute 'fixed-pitch nil 
                      :family "Droid Sans Mono" 
                      :weight 'normal 
                      :height 1.0) 
  (set-face-attribute 'variable-pitch nil 
                      :family "Droid Sans" 
                      :weight 'light
                      :height 1.0)
  ;; (set-face-attribute 'font-lock-comment-face nil :family "Monoid" :weight 'light :italic t :height 100)
  ;; (set-face-attribute 'org-level-1 nil :family "Liberation Serif" :height 180)
  ;; (set-face-attribute 'outshine-level-1 nil :family "Liberation Serif" :height 180)
  )

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook 
          (lambda () 
            (update-font-settings)))

;; Межстрочный интервал

(update-font-settings)

(setq-default line-spacing 1)

(provide 'шрифты-для-андрюши)
