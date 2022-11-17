(use-package dashboard
  :ensure t
  :bind (:map dashboard-mode-map
              ("C-g" . dashboard-refresh-buffer))
  :custom 
  (dashboard-startup-banner "~/Добро/lisp.png")
  (dashboard-banner-logo-title "Добро пожаловать в свободную систему!")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 3)
                     (bookmarks . 3)
                     (projects . 3)
                     ;;(agenda . 5)
                     (registers . 5)))
  (dashboard-item-names '(("Recent Files:" . "Недавно открытые файлы:")
                          ("Agenda for today:" . "Расписание на сегодня:")
                          ("Agenda for the coming week:" . "Расписание на неделю:")
                          ("Bookmarks:" . "Закладки:")
                          ("Projects:" . "Проекты:")
                          ("Registers:" . "Регистры")))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)                        
  (dashboard-set-navigator nil)
  (dashboard-navigator-buttons
   `(;; line1
     ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "homepage")))
      ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
      ("?" "" "?/h" #'show-help nil "<" ">"))
     ;; line 2
     ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
       "Linkedin"
       ""
       (lambda (&rest _) (browse-url "homepage")))
      ("⚑" nil "Show flags"
       (lambda (&rest _) (message "flag")) error))))
  (dashboard-footer-messages '("Сообщение в футере"))
  (dashboard-footer-icon (all-the-icons-octicon "dashboard"
                                                :height 1.1
                                                :v-adjust -0.05
                                                :face 'font-lock-keyword-face))
  :config
  (dashboard-setup-startup-hook)
  :init
  ;(dashboard-refresh-buffer)
  )

(provide 'пульт-управления)
