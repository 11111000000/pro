;;; про-интернет-сервисы.el --- Сеть и Интернет  -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация сетевых сервисов, браузеров и мессенджеров
;;; Code:

(require 'use-package)

;;;; Tor proxy

(require 'url)

(use-package use-proxy
  :ensure t
  :config
  ;; Set up proxy settings here
  (setq use-proxy-http-proxy "socks5h://127.0.0.1:9050")
  (setq use-proxy-https-proxy "socks5h://127.0.0.1:9050")
  (setq use-proxy-no-proxy "localhost|127.0.0.1|::1")
  
  (use-proxy-mode 1)

  ;; Key bindings to toggle proxy settings
  
  (global-set-key (kbd "C-c C-g") 'use-proxy-toggle-proxies-global))

;; Set the proxy for HTTP and HTTPS
;; (setq url-proxy-services
;;      '(("no_proxy" . "localhost, 127.0.0.1") ; Not to proxy local addresses
;;        ("http" . "socks5://127.0.0.1:9050")
;;        ("https" . "socks5://127.0.0.1:9050")))

                                        ;(setq url-http-timeout 30)


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

(setq-default browse-url-browser-function 'browse-url-chrome
              browse-url-new-window-flag t
              browse-url-generic-program "chromium")

;;;; EWW - Браузер на ELISP

(use-package eww
  :defer t
  :ensure t
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
  :ensure t
  :defer t
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

(defun my-browse-url-at-mouse (event)
  "Open URL at mouse point with Alt or Shift key modifier.
Use Firefox if Alt is pressed, Chrome if Shift is pressed."
  (interactive "e")
  (let* ((mouse-pos (event-start event))
        (url (thing-at-point 'url)))
    (cond
     ((and (equal (event-modifiers event) '(alt)) url) ;; Alt pressed
      (start-process "firefox" nil "firefox" url))
     ((and (equal (event-modifiers event) '(shift)) url) ;; Shift pressed
      (start-process "chrome" nil "google-chrome" url))
     (url
      (browse-url-default-browser url))))) ;; Open in default browser if no modifier

(global-set-key [mouse-1] 'my-browse-url-at-mouse) ;; Настраиваем левую кнопку мыши

;;;; Загрузка субтитров с Youtube

;; 1. Запрашивает URL видео на YouTube.
;; 2. Получает список доступных языков субтитров.
;; 3. Позволяет выбрать язык с помощью =consult=.
;; 4. Скачивает субтитры и сохраняет их в папку =~/Subtitles/=, создавая её при необходимости.

(require 'consult)

(defun my-download-youtube-subtitles ()
  "Download subtitles from a YouTube video using yt-dlp with Tor proxy."
  (interactive)
  (let* ((url (read-string "Enter YouTube video URL: "))
         (proxy "--proxy socks5://127.0.0.1:9050")
         (list-subs-cmd (format "yt-dlp %s --list-subs \"%s\"" proxy url))
         (subs-output (shell-command-to-string list-subs-cmd))
         (lang-list (my-parse-yt-dlp-subs-output subs-output))
         (selected-lang (consult--read lang-list :prompt "Choose subtitle language: "))
         (subs-directory (expand-file-name "~/Subtitles/"))
         (download-cmd (format "yt-dlp %s --write-subs --sub-lang %s --skip-download -o \"%s%%(title)s.%%(ext)s\" \"%s\""
                               proxy selected-lang subs-directory url)))
    ;; Создаем директорию, если она не существует
    (unless (file-directory-p subs-directory)
      (make-directory subs-directory t))
    ;; Скачиваем субтитры
    (message "Downloading subtitles...")
    (let ((exit-code (shell-command download-cmd)))
      (if (eq exit-code 0)
          (message "Subtitles downloaded to %s" subs-directory)
        (message "Failed to download subtitles. Please check the URL and try again.")))))

(defun my-parse-yt-dlp-subs-output (output)
  "Parse yt-dlp --list-subs OUTPUT and return a list of available subtitle languages."
  (let ((languages '()))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      ;; Парсим разделы с доступными субтитрами
      (while (re-search-forward "^\\[info\\] Available \\(automatic \\)?captions for .*:\n" nil t)
        ;; Пропускаем заголовок таблицы
        (forward-line 1)
        ;; Считываем таблицу субтитров
        (while (and (not (looking-at "^$"))
                    (not (eobp)))
          (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (when (string-match "^\\([a-zA-Z0-9-]+\\)[ \t]+" line)
              (let ((lang-code (match-string 1 line)))
                (push lang-code languages))))
          (forward-line 1))))
    (setq languages (delete-dups languages))
    (if languages
        (nreverse languages)
      (user-error "No subtitles available for this video"))))



(provide 'про-интернет-сервисы)
;;; про-интернет-сервисы.el ends here
