;;; про-время.el --- Время, календарь и оповещения -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Всё о времени в Emacs — удобный календарь, часы, мировое время, праздники, фазы луны,
;; напоминания и визуальное оформление.  Работает кроссплатформенно, минималистично и расширяемо.
;;
;; В этом модуле:
;; — Современное отображение времени в mode-line
;; — Календарь с русскоязычными подписями, праздниками и лунными фазами
;; — Мировое время для нескольких городов
;; — Напоминания и простые оповещения (appt)
;; — Конвертеры дат
;; — Эстетика: фиксированный шрифт и масштаб, поддержка тёмных/светлых тем
;;

;;; Code:

;;;; 1. Часы и отображение времени в Emacs

;; Показывать часы в mode-line (см. настройку format-time-string ниже)
(require 'time)
(setq display-time-24hr-format t)             ;; 24-часовой формат времени
(setq display-time-day-and-date t)            ;; Показывать дату (не только часы)
(setq display-time-format "%H:%M %a %d.%m")   ;; Например: 13:52 Пн 31.07
(display-time-mode 1)                         ;; Включить mode-line часы

;; Опционально: показывать уровень батареи, если на ноутбуке
(when (and (require 'battery nil t)
           (battery))
  (display-battery-mode 1))

;;;; 2. Календарь

(require 'calendar)
(require 'holidays)
(require 'lunar)

;; Функция для более красивого шрифта календаря
(defun set-calendar-font ()
  "Аккуратный фиксированный шрифт в календаре."
  (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
  (buffer-face-mode t))

(add-hook 'calendar-mode-hook #'set-calendar-font)

;; Календарь по умолчанию в понедельник
(setq calendar-week-start-day 1)

;; Настройка формата: неделя, месяц, год
(setq calendar-date-style 'european) ;; dd/mm/yyyy для России/Европы

;; Русская локализация дней недели
(setq calendar-day-name-array ["Воскресенье" "Понедельник" "Вторник" "Среда" "Четверг" "Пятница" "Суббота"])
(setq calendar-day-abbrev-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"])
(setq calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май" "Июнь"
                                 "Июль" "Август" "Сентябрь" "Октябрь" "Ноябрь" "Декабрь"])

;; Если нужно, отмечаем текущий день (face можно подстроить)
(setq calendar-mark-today t)

;;;; 3. Праздники и выходные

;; Национальные (российские) праздники по умолчанию + лунные праздники
(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Новый год")
        (holiday-fixed 1 7 "Рождество (православное)")
        (holiday-fixed 2 23 "День защитника Отечества")
        (holiday-fixed 3 8 "Международный женский день")
        (holiday-fixed 5 1 "Праздник Весны и Труда")
        (holiday-fixed 5 9 "День Победы")
        (holiday-fixed 6 12 "День России")
        (holiday-fixed 11 4 "День народного единства")))

(setq holiday-other-holidays
      '((holiday-fixed 12 25 "Рождество (католическое)")
        (holiday-float 10 0 -1 "Последнее воскресенье октября — конец летнего времени")
        (holiday-float 3 0 -1 "Последнее воскресенье марта — начало летнего времени")
        (holiday-easter-etc 0 "Пасха (по григорианскому календарю)")
        (holiday-lunar-phases)))  ;; Фазы луны

;; Подключить свои источники праздников
(setq calendar-holidays (append holiday-general-holidays
                                holiday-other-holidays
                                holiday-local-holidays))

;;;; 4. Лунные фазы

;; Функция: показать фазы луны сегодня (или на любую дату)
(defun show-today-moon-phases ()
  "Показать лунные фазы сегодня (или ближайшие лунные дни)."
  (interactive)
  (let* ((calendar-date (calendar-current-date))
         (phase (lunar-phase calendar-date)))
    (message "Фаза луны: %s" (nth 3 phase))))

;; Быстрое горячее открытие календаря и просмотр лунных фаз
(global-set-key (kbd "C-c k") #'calendar)
(global-set-key (kbd "C-c l") #'show-today-moon-phases)

;;;; 5. Мировое время (world clock)

(require 'time)
(require 'time-date)
(require 'timeclock)
(require 'time)

;; Добавьте ваши города и смещения (название, зона)
(setq display-time-world-list
      '(("Europe/Moscow"      "Москва")
        ("Europe/London"      "Лондон")
        ("America/New_York"   "Нью-Йорк")
        ("Asia/Tokyo"         "Токио")
        ("Europe/Berlin"      "Берлин")
        ("Asia/Novosibirsk"   "Новосибирск")))

(setq display-time-world-time-format "%a %d.%m  %H:%M %Z %s") ;; День Дата Время Зона

;; Глобальная горячая клавиша для списка мировых часов
(global-set-key (kbd "C-c w") #'display-time-world)

;;;; 6. Напоминания и простые оповещения (appt)

(require 'appt)
(appt-activate 1)  ;; включить обработку напоминаний, если они созданы
(setq appt-audible t)          ;; звуковое напоминание
(setq appt-display-interval 10) ;; каждые 10 минут до события
(setq appt-message-warning-time 15) ;; за сколько минут предупреждать
(setq appt-display-format 'window)  ;; как показывать: 'echo 'window 'both

;; Можно интегрировать с org-mode для org-appointments (расширение):
(when (require 'org-agenda nil t)
  (org-agenda-to-appt)
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt))

;;;; 7. Переводы и вычисления дат/времени

;; Быстрые функции перевода дат: юлианская/григорианская/эпоха unix
(defun про/convert-unix-to-date (unixtime)
  "Перевести unixtime (число секунд от 1970-01-01) в строку даты."
  (interactive "nВремя в формате Unix (секунды с 1970): ")
  (message "%s" (format-time-string "%Y-%m-%d %A %H:%M:%S" (seconds-to-time unixtime))))

(defun про/current-unixtime ()
  "Показать текущее unixtime."
  (interactive)
  (message "%d" (float-time (current-time))))

(global-set-key (kbd "C-c u") #'про/current-unixtime)

;;;; 8. Эстетика и удобства

;; Обеспечиваем масштаб и минимализм в календаре
(setq calendar-view-diary-initially-flag nil)
(setq calendar-mark-holidays-flag t)
(setq calendar-make-frame-available t)
(setq calendar-remove-frame-by-deleting t)

;; Можно визуально увеличить календарь (если нужно)
;; (add-hook 'calendar-mode-hook (lambda () (text-scale-set 1)))

;; Для красивых рамок в терминале
(when (boundp 'window-divider-mode)
  (add-hook 'calendar-mode-hook #'window-divider-mode))

;;;; 9. Быстрые команды пользователя (краткая шпаргалка)

;; Мнемонические бинды:
;; C-c k — календарь
;; C-c w — мировое время
;; C-c l — текущая фаза луны
;; C-c u — текущее unixtime

;;; Финал

(provide 'про-время)

;;; про-время.el ends here
