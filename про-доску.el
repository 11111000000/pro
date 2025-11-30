;;; про-доску.el --- Пульт управления -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;; Дашборд

(require 'all-the-icons)
(require 'calendar)
(require 'holidays)
(require 'shaoline)
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
         ;; Лунная фаза (используем lunar.el + shaoline при наличии)
         (moon
          (let* ((icons (if (boundp 'shaoline-moon-icons)
                            shaoline-moon-icons
                          ["🌑" "🌒" "🌓" "🌔" "🌕" "🌖" "🌗" "🌘"]))
                 (jd (if (fboundp 'shaoline--jd-now)
                         (shaoline--jd-now)
                       ;; approximate Julian day from current time: JD = Unix/86400 + 2440587.5
                       (+ (/ (float-time (current-time)) 86400.0) 2440587.5)))
                 (synodic-month (if (boundp 'shaoline--synodic-month)
                                    shaoline--synodic-month
                                  29.530588853))
                 (next-new (lunar-new-moon-on-or-after jd))
                 (prev-guess (lunar-new-moon-on-or-after (- jd synodic-month)))
                 (prev-new (if (> prev-guess jd)
                               (lunar-new-moon-on-or-after (- jd (* 2 synodic-month)))
                             prev-guess))
                 (age (- jd prev-new))
                 (idx (min 7 (max 0 (floor (* age (/ 8.0 synodic-month)))))))
            (if (and (integerp idx) (<= 0 idx) (< idx 8))
                (aref icons idx)
              "☾"))))
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
         :map dashboard-mode-map
         ("C-g" . keyboard-quit))
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
  ;; Футер обновляем динамически при открытии/обновлении Dashboard.
  (dashboard-footer-messages (list (про-доску/футер)))
  (dashboard-footer-icon (if window-system (all-the-icons-octicon "dashboard"
                                                                  :height 1.1
                                                                  :v-adjust -0.05
                                                                  :face 'font-lock-keyword-face) "."))
  :config
  (defun про-доску/обновить-футер ()
    "Сформировать и установить динамический футер Dashboard."
    (setq dashboard-footer-messages (list (про-доску/футер))))
  (add-hook 'dashboard-mode-hook #'про-доску/обновить-футер)
  (advice-add 'dashboard-refresh-buffer :before #'про-доску/обновить-футер)
  :init
  ;;(dashboard-refresh-buffer)
  )

(provide 'про-доску)
;;; про-доску.el ends here
