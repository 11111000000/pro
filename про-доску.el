;;; про-доску.el --- Пульт управления -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Дашборд

(use-package dashboard
  :ensure t
  :defer t
  :defines (dashboard-mode-map)
  :bind (
         ("<f5>" . dashboard-refresh-buffer)
         ("C-<f5>" . dashboard-refresh-buffer)
         :map dashboard-mode-map
         ("C-g" . dashboard-refresh-buffer))
  :hook ((dashboard-mode-hook . variable-pitch-mode))
  :custom
  (dashboard-startup-banner "~/pro/lisp.png")
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
     ((,(if window-system (all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0) nil)
       "Homepage"
       "Browse homepage"
       (lambda (&rest _) (browse-url "homepage")))
      ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
      ("?" "" "?/h" #'show-help nil "<" ">"))
     ;; line 2
     ((,(if window-system (all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0) nil)
       "Linkedin"
       ""
       (lambda (&rest _) (browse-url "homepage")))
      ("⚑" nil "Show flags"
       (lambda (&rest _) (message "flag")) error))))
  (dashboard-footer-messages '("Сообщение в футере"))
  (dashboard-footer-icon (if window-system (all-the-icons-octicon "dashboard"
                                                               :height 1.1
                                                               :v-adjust -0.05
                                                               :face 'font-lock-keyword-face) "."))
  :init
  (dashboard-refresh-buffer))

(provide 'про-доску)
;;; про-доску.el ends here
