;;; про-интернет-новости.el --- Ленты новостей RSS
;;; Commentary:
;;; Code:
;;;; Elfeed

(use-package elfeed
  :ensure t
  :defines (elfeed-feeds elfeed-search-filter)
                                        ;:hook ((elfeed-search-mode . variable-pitch-mode)))
  :config
  (setq elfeed-feeds
       '(("https://lenta.ru/rss" news russia)
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


(provide 'про-интернет-новости)
;;; про-интернет-новости.el ends here
