;;; про-rss.el --- Seed: RSS-ридер для Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; Поддержка Elfeed как минималистичного RSS-агрегатора. Подключение вручную списком фидов.
;;; Code:

(use-package elfeed
  :ensure t
  :commands (elfeed)
  :config
  (setq elfeed-feeds
        '("https://planet.emacslife.com/atom.xml"
          "https://emacs.meta.stackexchange.com/feeds"
          "https://habr.com/ru/rss/all/all/")))

(global-set-key (kbd "C-c f") #'elfeed)

(message "Elfeed seed (RSS) активирован.")

(provide 'про-rss)
;;; про-rss.el ends here
