;;; интернет-новости.el --- Ленты новостей RSS
;;; Commentary:
;;; Code:
;;;; Elfeed
(use-package elfeed
  :ensure t
  :custom (elfeed-feeds '("http://nullprogram.com/feed" ("https://www.youtube.com/feeds/videos.xml?channel_id=UC8tThli1ZY7LW5Dxqr3Y0jA"
                                                         video) ;; Emacs macros channel
                          "hnrss.org/replies/?id=celeritascelery" "hnrss.org/newest?points=100"
                          "https://www.reddit.com/r/emacs.rss" "https://irreal.org/blog/?feed=rss2"
                          "https://sachachua.com/blog/category/emacs-news/feed")))

(provide 'интернет-новости)
;;; интернет-новости.el ends here
