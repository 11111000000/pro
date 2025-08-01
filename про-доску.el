;;; про-доску.el --- Пульт управления -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Дашборд

(require 'all-the-icons)
(require 'calendar)
(require 'holidays)
(require 'lunar)

(defun про-доску/футер (&optional _)
  "Вернуть красивый футер: дата/время, праздник и фаза луны (иконка).
_ — игнорируемый аргумент, нужен для совместимости с dashboard."
  (let* ((now (current-time))
         (date-str (format-time-string "%H:%M, %d %B %Y (%a)" now)) ; пример: 15:12, 5 июня 2024 (Ср)
         ;; Получить праздники (calendar/holidays)
         (cal-date (calendar-current-date))
         (holidays-today
          (let ((hs (calendar-check-holidays cal-date)))
            (and hs (mapconcat #'identity hs ", "))))
         ;; Лунная фаза
         (moon-icons ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"])
         ;; moon phase: robust, use integer date, as in shaoline
         (moon-idx
          (let* ((abs-now (float (calendar-absolute-from-gregorian cal-date)))
                 (synodic-month 29.530588853)
                 (next-new (lunar-new-moon-on-or-after abs-now))
                 (prev-new (- next-new synodic-month))
                 (age (- abs-now prev-new)))
            (mod (floor (* age (/ 8.0 synodic-month))) 8)))
         (moon (if (and (integerp moon-idx) (<= 0 moon-idx) (< moon-idx 8))
                   (aref moon-icons moon-idx)
                 "☾")))
    (concat
     moon
     "  "
     (if holidays-today
         (concat holidays-today " • ")
       "")
     date-str)))

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
  ;; --- Красивый футер с датой, праздником и луной ---

  (dashboard-footer-messages (list (про-доску/футер)))
  (dashboard-footer-icon (if window-system (all-the-icons-octicon "dashboard"
                                                                  :height 1.1
                                                                  :v-adjust -0.05
                                                                  :face 'font-lock-keyword-face) "."))
  :init
  ;;(dashboard-refresh-buffer)
  )

(provide 'про-доску)
;;; про-доску.el ends here
