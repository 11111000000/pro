;;; про-интернет-сервисы.el --- Сеть и Интернет  -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация сетевых сервисов, браузеров и мессенджеров
;;; Code:

(require 'use-package)

;;;; Ускорение работы SSH

(require 'tramp)
(require 'tramp-sh)

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
  :defer t 
  :config)

;;;;; Нумерация ссылков

(use-package ace-link
  :defer t 
  :ensure t
  :after eww
  :defer t
  :commands (eww-back-url eww-forward-url ace-link-eww)
  :bind (:map eww-mode-map
                ("<" . eww-back-url)
                (">" . eww-forward-url)
                ("C-c f" . ace-link-eww)))

(use-package eww-lnum
  :defer t 
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
  :defer t 
  :ensure t
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
                ("C-S-R" . w3m-reload-this-page))
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


;;;; Карты OSM

(use-package osm
  :defer t 
  :ensure t)

;;;; Погода

(use-package wttrin
  :defer t 
  :ensure t
  :defines (wttrin-default-accept-language wttrin-default-cities)
  :commands (wttrin)
  :bind (("<f1> W" . wttrin))
  :init
  (setq wttrin-default-accept-language '("Accept-Language" . "ru-RU,ru")
       wttrin-default-cities '("Moscow"
                               "Novosibirsk"
                               "Krasnoyarsk"
                               "Irkutsk"
                               "Angarsk"
                               "Voronezh"
                               "Rossosh")))

;;;; Перевод валют

(require 'url)

(defun usd-to-rub (sum)
  "Преобразует сумму в долларах SUM в рубли.
используя текущий обменный курс с сервера."
  
  (progn
    (message "aaa %f" sum)
    (url-retrieve "https://api.exchangerate-api.com/v4/latest/USD"
                  (lambda (status res)
                    (message "bbb %f" sum)
                    ;; Обработка возможных ошибок при запросе.
                    (if (plist-get status :error)
                        (message "Не удалось получить данные о курсе валют.")
                      (goto-char url-http-end-of-headers)
                      ;; Парсинг JSON-ответа
                      (let* ((json-object-type 'hash-table)
                            (json-array-type 'list)
                            (json-key-type 'string)
                            (response (json-read))
                            (rates (gethash "rates" response))
                            (rub-rate (gethash "RUB" rates)))
                        (if rub-rate
                            (message "Сумма в RUB: %.2f" (* sum rub-rate))
                          (message "Не найден курс RUB в ответе.")))))
                  (list sum))))

(provide 'про-интернет-сервисы)
;;; про-интернет-сервисы.el ends here
