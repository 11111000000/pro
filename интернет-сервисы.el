;;; интернет-сервисы.el --- Сеть и Интернет
;;; Commentary:
;; Конфигурация сетевых сервисов, браузеров и мессенджеров
;;; Code:
;;;; Ускорение работы SSH

(setq tramp-ssh-controlmaster-options t)
(setq tramp-chunksize 500)
(setq tramp-default-method "ssh")

(setq vc-ignore-dir-regexp
             (format "\\(%s\\)\\|\\(%s\\)"
                     vc-ignore-dir-regexp
                     tramp-file-name-regexp))

;;;; URL открывается в текстовом браузере в новой вкладке или в Хроме.

(setq-default browse-url-browser-function 'eww-browse-url
              browse-url-new-window-flag t
              browse-url-generic-program "chromium")

;;;; EWW - Браузер на ELISP

(use-package eww
  :bind (:map eww-mode-map
                ("f"))
  :config
  
  )

;;;;; Нумерация ссылков

(use-package eww-lnum
  :ensure t
  :demand t
  :after eww
  :bind (
         :map eww-mode-map
                ("C-f" . forward-char)
                ("f" . eww-lnum-follow)
                ("F" . eww-lnum-follow)))

;;;; W3M - альтернативный текстовый браузер

(use-package w3m
  :ensure t
  :defer t
  :hook (w3m-mode . w3m-lnum-mode)
  :bind (:map w3m-mode-map
              ("C-<tab>" . w3m-tab-next-buffer)
              ("C-<iso-lefttab>" . w3m-tab-previous-buffer)
              ("C-w" . w3m-delete-buffer)
              ("M-n" . w3m-tab-next-buffer)
              ("M-p" . w3m-tab-previous-buffer)
              ("<S-return>" . w3m-view-this-url-background-session)
              ("<M-return>" . w3m-view-this-url-background-session)
              ("B" . w3m-view-previous-page)
              ("F" . w3m-view-next-page)
              ("<M-left>" . w3m-view-previous-page)
              ("<M-right>" . w3m-view-previous-page)
              ("F" . w3m-view-next-page)
              ("M-s" . nil)
              ("<XF86Back>" . nil)
              ("<XF86Forward>" . nil)
              ("C-r" . w3m-reload-this-page)
              ("C-S-R" . w3m-reload-this-page)
              )
  :config

  (autoload 'w3m-browse-url "w3m" ">" t)

  (setq-default w3m-use-cookies t
                browse-url-new-window-flag t
                w3m-show-graphic-icons-in-header-line t
                w3m-display-inline-images t
                w3m-show-graphic-icons-in-mode-line t
                w3m-user-agent "Mozilla/5.0 (Linux; U; Android 2.3.3; zh-tw; HTC_Pyramid Build/GRI40) AppleWebKit/533.1 (KHTML, like Gecko) Version/4.0 Mobile Safari/533."
                w3m-session-load-always t
                w3m-session-autosave t
                w3m-session-load-last-sessions t))

;;;; HTTP-запросы

;; (use-package plz
;;   :init (установить-из-репы :repo "alphapapa/plz")
;;   ;; :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
;;   )

;; (use-package ement
;;   :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

;;;; Карты OSM

(use-package osm
  :ensure t)

;;;; Погода

(use-package wttrin
  :ensure t)

(provide 'интернет-сервисы)
;;; интернет-сервисы.el ends here
