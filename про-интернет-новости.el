;;; про-интернет-новости.el --- Ленты новостей RSS -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация лент новостей
;;; Code:
;;;; Elfeed

(use-package elfeed
  :defer t
  :ensure t
  :defines (elfeed-feeds elfeed-search-filter)
                                        ;:hook ((elfeed-search-mode . variable-pitch-mode)))
  :config
  (setq elfeed-feeds
       '(("https://www.finam.ru/analysis/nslent/rsspoint/" news russia market analytics)
         ("https://www.finam.ru/analysis/conews/rsspoint/" news russia analytics)
         ("https://www.finam.ru/international/advanced/rsspoint/" news world market analytics)
         ("https://lenta.ru/rss" news russia)
         ("https://actualidad.rt.com/feed" news russia)
         ("http://static.feed.rbc.ru/rbc/logical/footer/news.rss" news)
         ("https://meduza.io/rss/all" news russia)
         ("http://newsbabr.com/rss/news.xml" news irkutsk)
         ("https://www.irk.ru/news.rss" news irkutsk)
         ("https://postnauka.ru/feed" science)
         ("https://gorky.media/feed/" science)
         ("http://news.rambler.ru/rss/world/" news world)
         ("http://news.yandex.ru/index.rss" news world)
         ("http://www.aif.ru/rss/all.php" news world russia)
         ("https://www.vz.ru/rss.xml" news russia)
         ("https://www.consultant.ru/rss/hotdocs.xml" news law russia)
         ("https://www.consultant.ru/rss/fd.xml" law russia)
         ("https://www.consultant.ru/rss/nw.xml" law russia)
         ("http://tayga.info/all.rss" news russia irkutsk)
         ("http://nullprogram.com/feed" news it)
         ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8tThli1ZY7LW5Dxqr3Y0jA" video)
         ("hnrss.org/replies/?id=celeritascelery" news it)
         ("hnrss.org/newest?points=100" news it)
         ("https://www.reddit.com/r/emacs.rss" news it emacs)
         ("https://irreal.org/blog/?feed=rss2" news it)
         ("https://sachachua.com/blog/category/emacs-news/feed" news it emacs))
       elfeed-search-filter "@2-days-ago +unread"))

;;;; Получить картину дня из elfeed

(require 'elfeed)

(defun elfeed-список-новостей-за (секунды)
  "Возвращает список записей из лент за СЕКУНДЫ."
  (let* ((время-сейчас (current-time))
        (времени-прошло (time-subtract время-сейчас (seconds-to-time секунды)))
        (результат '()))
    (maphash
     (lambda (_key entry)
       (let ((время-записи (seconds-to-time (elfeed-entry-date entry))))
         (when (and (time-less-p времени-прошло время-записи)
                 (time-less-p время-записи время-сейчас))
           (push (elfeed-entry-title entry) результат))))
     elfeed-db-entries)
    (reverse результат)))

(require 'subr-x)

(defun elfeed-обновить-и-выполнить (колбэк)
  "Update elfeed and call КОЛБЭК with the summary of today's entries."
  (let (хук-когда-обновятся-ленты)
    (setq хук-когда-обновятся-ленты (lambda ()
                                     (remove-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
                                     (funcall колбэк)
                                     ))
    (add-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
    (elfeed-update)))

;;;; Обработка новостей

(require 'chatgpt-shell)

(defvar промт-новости-по-умолчанию "убери рекламу, систематизируй и кратко суммаризируй все новости, добавь аналитику на главные темы")

(defun новости-за-время (часов-новостей &optional промт)
  "Рассказывает новости Elfeed за ЧАСОВ-НОВОСТЕЙ.  Можно добавить ПРОМТ."
  (interactive)
  (let* ((список-новостей (elfeed-список-новостей-за (* 3600 часов-новостей)))
        (текст-новостей (string-join список-новостей)))
    (with-current-buffer (or (chatgpt-shell--primary-buffer) (current-buffer))
      (chatgpt-shell-send-to-buffer
       (concat "Вот события за "
              (number-to-string часов-новостей)
              " часа. "
              (or промт промт-новости-по-умолчанию)
              текст-новостей)
       nil))))

(defun новости-за-час ()
  "Рассказывает новости Elfeed за час."
  (interactive)
  (новости-за-время 1))

(defun новости-за-сутки ()
  "Рассказывает новости Elfeed за сутки."
  (interactive)
  (новости-за-время 24))

(defun новости-за-день ()
  "Рассказывает новости Elfeed за день."
  (interactive)
  (новости-за-время 12))

(defun новости-куплет-за-сутки ()
  "Поёт новости Elfeed за сутки."
  (interactive)
  (новости-за-время 24
                    "Сначала сгруппируй новости по одной теме, а затем создай лёгкий, красивый и рифмованный поэтический пересказ:"))


(defun новости-смешное-за-час ()
  "Рассказывает новости Elfeed за день."
  (interactive)
  (новости-за-время 1
                    "Перескажи только смешные или просто несуразные и курьёзные новости и суммаризируй, простым текстом, без Markdown, только суммаризацию, неформальным, лёгким языком :"))

(defun новости-смешное-за-сутки ()
  "Рассказывает смешные новости Elfeed за сутки."
  (interactive)
  (новости-за-время 24
                    "Перескажи только смешные или просто несуразные и курьёзные новости и суммаризируй, простым текстом, без Markdown, только суммаризацию, неформальным, лёгким языком :"))


(defun новости-войны-за-сутки ()
  "Рассказывает новости войны за сутки."
  (interactive)
  (новости-за-время 24
                    "Перескажи кратко только военные новости, разбей на группы по смыслу, систематизируй и суммаризируй, простым текстом, без Markdown:"))


(defun новости-анализ-за-сутки ()
  "Рассказывает анализ новостей за сутки."
  (interactive)
  (новости-за-время 24
                    "убери рекламу, и сделай анализ и предсказание по заголовкам новостей: напиши только результат анализа, простым текстом, без Markdown:"))


(provide 'про-интернет-новости)
;;; про-интернет-новости.el ends here
