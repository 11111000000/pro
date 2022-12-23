;;; сеть.el --- Сеть и Интернет
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


;;;; Telegram

(use-package telega
  :ensure t
  :custom ((telega-use-docker t)
           (telega-use-images t)
           ;; (telega-emoji-use-images t)
           (telega-emoji-font-family "Noto Color Emoji")
           )
  :hook ((telega-root-mode . telega-notifications-mode)         
         (telega-load-hook . global-telega-url-shorten-mode))
  :config    
  )

;;;; URL открывается в текстовом браузере в новой вкладке или в Хроме.

(setq-default browse-url-browser-function 'eww-browse-url
              browse-url-new-window-flag t
              browse-url-generic-program "chromium")


;;;; EWW - Браузер на ELISP

;;;;; Нумерация ссылков 

(use-package eww-lnum
  :ensure t
  :demand t
  :after eww
  :bind
  (
   :map eww-mode-map
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

(use-package plz
  ;; :quelpa (plz :fetcher github :repo "alphapapa/plz.el")
  )

;; (use-package ement
;;   :quelpa (ement :fetcher github :repo "alphapapa/ement.el"))

;;;; Slack

;; (use-package helm-slack :after (slack)) ;; optional

;; (use-package slack
;;   :ensure t
;;   :commands (slack-start)
;;   :init
;;   (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
;;   (setq slack-prefer-current-team t)
;;   :config

;;   (slack-register-team
;;    :name "test"
;;    :default t
;;    :token "xoxc-2947284578897-2934644622146-2927957344550-66f45a073a88f7f4adfc1df11f59d396fd5a2a67fadaa47bc0597ce9e24f8b58"
;;    ;;:subscribed-channels '(hoge fuga)
;;    )

;;   ;; (evil-define-key 'normal slack-info-mode-map
;;   ;;   ",u" 'slack-room-update-messages)
;;   ;; (evil-define-key 'normal slack-mode-map
;;   ;;   ",c" 'slack-buffer-kill
;;   ;;   ",ra" 'slack-message-add-reaction
;;   ;;   ",rr" 'slack-message-remove-reaction
;;   ;;   ",rs" 'slack-message-show-reaction-users
;;   ;;   ",pl" 'slack-room-pins-list
;;   ;;   ",pa" 'slack-message-pins-add
;;   ;;   ",pr" 'slack-message-pins-remove
;;   ;;   ",mm" 'slack-message-write-another-buffer
;;   ;;   ",me" 'slack-message-edit
;;   ;;   ",md" 'slack-message-delete
;;   ;;   ",u" 'slack-room-update-messages
;;   ;;   ",2" 'slack-message-embed-mention
;;   ;;   ",3" 'slack-message-embed-channel
;;   ;;   "\C-n" 'slack-buffer-goto-next-message
;;   ;;   "\C-p" 'slack-buffer-goto-prev-message)
;;   ;;  (evil-define-key 'normal slack-edit-message-mode-map
;;   ;;   ",k" 'slack-message-cancel-edit
;;   ;;   ",s" 'slack-message-send-from-buffer
;;   ;;   ",2" 'slack-message-embed-mention
;;   ;;   ",3" 'slack-message-embed-channel)
;; )

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

;;;; Карты OSM

(use-package osm
  :ensure t)

(provide 'сеть)
;;; сеть.el ends here
