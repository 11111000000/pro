;;; про-искусственный-интеллект.el --- Искусственный Интеллект -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)
;;; Commentary:
;; Конфигурация нейросетевых сервисов
;;; Code:

(require 'установить-из)
(require 'cape)

;;;; Настройка openapi-key

(if (boundp 'proxyapi-key)
    (progn
      (setq-default openai-key proxyapi-key)
      (setq-default chatgpt-shell-openai-key proxyapi-key)
      (setq-default dall-e-shell-openai-key proxyapi-key)))

;;;; Библиотека поддержки Нейросетей OpenAI

;; (use-package openai                       ;
;;   :init
;;   (установить-из :repo "11111000000/openai")
;;   :custom
;;   (openai-base-url "https://api.proxyapi.ru/openai/v1"))

;;;; REPL для разных нейросетей. ChatGPT Shell

(use-package chatgpt-shell
  :init (установить-из :repo "xenodium/chatgpt-shell")
  :bind (("s-a" . chatgpt-shell-prompt))
  :custom (
          (chatgpt-shell-model-versions '("gpt-4o"
                                          "gpt-4-0125-preview"
                                          "gpt-4-turbo-preview"
                                          "gpt-4-1106-preview"
                                          "gpt-4-0613"
                                          "gpt-4"
                                          "gpt-3.5-turbo-16k-0613"
                                          "gpt-3.5-turbo-16k"
                                          "gpt-3.5-turbo-0613"
                                          "gpt-3.5-turbo"))
          (chatgpt-shell-api-url-base  "https://api.proxyapi.ru/openai")
          (dall-e-shell--url "https://api.proxyapi.ru/v1/images/generations")))

;;;; Поддержка локальной нейросети LLAMA

(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "Russian")
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "codellama" :embedding-model "codellama")))

;;;; Дополните кода нейросетью Codeium

(defun запустить-codeium()
  "Enable codeium."
  (interactive)
  (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

;; Codeium

;; настройки для программного интерфейса Codeium

(use-package codeium
  :init (установить-из :repo "Exafunction/codeium.el")
  :bind
  ("C-c <tab>" . дополнить-codeium)
  ("M-S-<iso-lefttab>" . дополнить-codeium)
  :config

  (defalias 'дополнить-codeium
    (cape-capf-interactive #'codeium-completion-at-point))

  (setq use-dialog-box nil)

  ;; используй M-x codeium-diagnose, чтобы увидеть API/поля, которые будут отправлены на локальный языковой сервер
  (setq codeium-api-enabled
       (lambda (api)
         (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))

  ;; Для отдельного буфера можно настроить так:
  ;; (add-hook 'python-mode-hook
  ;;     (lambda ()
  ;;         (setq-local codeium/editor_options/tab_size 4)))

  (setq codeium/document/text
       (lambda ()
         (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max)))))
  (setq codeium/document/cursor_offset
       (lambda ()
         (codeium-utf8-byte-length
          (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))))

;;;; Распознавание голоса. Whisper

(use-package whisper
  :init (установить-из :repo "natrys/whisper.el")
  :bind ("M-<f5>" . whisper-run)
  :config
  (setq whisper-install-directory (expand-file-name "~/.emacs.d/")
       whisper-model "medium"
       whisper-language "ru"
       whisper-translate nil
       whisper-use-threads (/ (num-processors) 2)))


;;;; Получить картину дня из elfeed

(require 'elfeed)

(require 'chatgpt-shell)

(defun elfeed-entries-last-seconds (seconds)
  "Return a list of titles of elfeed entries that are from the last hour."
  (let* ((current-time (current-time))
        (time-ago (time-subtract current-time (seconds-to-time seconds)))
        (result '()))
    (maphash
     (lambda (_key entry)
       (let ((entry-time (seconds-to-time (elfeed-entry-date entry))))
         (when (and (time-less-p time-ago entry-time)
                 (time-less-p entry-time current-time))
           (push (elfeed-entry-title entry) result))))
     elfeed-db-entries)
    (reverse result)))

(defun elfeed-entries-today ()
  "Return a list of titles of elfeed entries that are from today."
  (let ((today (format-time-string "%Y-%m-%d"))
       (result '()))
    (maphash
     (lambda (_key entry)
       (let ((date (seconds-to-time (elfeed-entry-date entry))))
         (when (equal (format-time-string "%Y-%m-%d" date) today)
           (push (elfeed-entry-title entry) result))))
     elfeed-db-entries)
    (reverse result)))

(defun concat-list-to-string (list)
  "Concatenate a LIST of strings into a single string with spaces in between."
  (mapconcat 'identity list ";"))

(defun elfeed-update-and-run-callback (callback)
  "Update elfeed and call CALLBACK with the summary of today's entries."
  (let* (хук-когда-обновятся-ленты)
    (setq хук-когда-обновятся-ленты (lambda (hook-level)
                                     (remove-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
                                     (funcall callback)))
    (add-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
    (elfeed-update)))

(defun новости-за-время (hours)
  "Рассказывает новсти Elfeed за HOURS."
  (interactive)
  (elfeed-update-and-run-callback
   (lambda ()
     (let* ((summary (concat-list-to-string (elfeed-entries-last-seconds (* 3600 hours)))))
       (with-current-buffer (chatgpt-shell--primary-buffer)
         (chatgpt-shell-send-to-buffer
          (concat "Вот события за " (number-to-string hours) " часа, систематизируй и суммаризируй, отфильтруй спам и рекламу, простым текстом, без Markdown:" summary)
          nil))))))

(defun новости-за-час ()
  "Рассказывает новсти Elfeed за час."
  (interactive)
  (новости-за-время 1))

(defun новости-за-сутки ()
  "Рассказывает новсти Elfeed за сутки."
  (interactive)
  (новости-за-время 24))

(defun новости-за-день ()
  "Рассказывает новсти Elfeed за день."
  (interactive)
  (новости-за-время 12))

(provide 'про-искусственный-интеллект)

;;; про-искусственный-интеллект.el ends here

