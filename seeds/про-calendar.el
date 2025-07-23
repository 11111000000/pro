;;; про-calendar.el --- Seed: Календарь, расписания, праздники -*- lexical-binding: t -*-
;;; Commentary:
;; Подключает встроенный календарь, синхронизацию праздников и org-appointments для GTD.
;;; Code:

;; Встроенный календарь
(global-set-key (kbd "C-c C") #'calendar)

;; Показать российские (или ваши) праздники
(setq calendar-holidays
      (append holiday-local-holidays
              holiday-other-holidays
              holiday-general-holidays
              '((holiday-fixed 1 1 "Новый год")
                (holiday-fixed 1 7 "Рождество православное")
                (holiday-fixed 2 23 "День защитника Отечества")
                (holiday-fixed 3 8 "8 марта")
                (holiday-fixed 5 1 "День труда")
                (holiday-fixed 5 9 "День победы")
                (holiday-fixed 6 12 "День России")
                (holiday-fixed 11 4 "День народного единства"))))

;; org-appointments интеграция (мини-напоминания)
(with-eval-after-load 'org
  (require 'org-agenda)
  (setq org-agenda-files (list (expand-file-name "../org" user-emacs-directory)))
  (setq org-agenda-include-diary t))

(global-set-key (kbd "C-c a") #'org-agenda)

(message "Календарь и org-напоминания активированы.")

(provide 'про-calendar)
;;; про-calendar.el ends here
