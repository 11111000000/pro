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
  :custom (
          (chatgpt-shell-model-versions '("gpt-4o-mini"
                                          "o1-mini"
                                          "o1-preview"
                                          "gpt-4o"
                                          "gpt-4o-2024-08-06"
                                          "gpt-4-turbo"
                                          "gpt-4"
                                          "gpt-3.5-turbo-0125"
                                          "gemini-1.5-pro"
                                          "gemini-1.5-flash"
                                          "claude-3-opus-20240229"))
          (chatgpt-shell-api-url-base  "https://api.proxyapi.ru/openai")
          (dall-e-shell--url "https://api.proxyapi.ru/v1/images/generations"))
  :config

  ;; Поддержка блоков Org-мод
  ;; Пример:   #+begin_src chatgpt-shell :version "gpt-4o" :system "результат в формате org-mode" :context emacs
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)
  )


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
    (setq хук-когда-обновятся-ленты (lambda (уровень-вызова)
                                     (remove-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
                                     (funcall колбэк)
                                     ))
    (add-hook 'elfeed-update-hooks хук-когда-обновятся-ленты)
    (elfeed-update)))

(defvar промт-новости-по-умолчанию "систематизируй и кратко суммаризируй новости, добавь ироничный комментарий в конце, в котором упомяни несколько ярких новостей. Отдельным блоком сделай все новости Иркутска и Ангарска")

(defun новости-за-время (часов-новостей &optional промт)
  "Рассказывает новости Elfeed за ЧАСОВ-НОВОСТЕЙ.  Можно добавить ПРОМТ."
  (interactive)
  (elfeed-обновить-и-выполнить
   (lambda ()
     (let* ((список-новостей (elfeed-список-новостей-за (* 3600 часов-новостей)))
           (текст-новостей (string-join  список-новостей)))
       (with-current-buffer (or (chatgpt-shell--primary-buffer) (current-buffer))
         (chatgpt-shell-send-to-buffer
          (concat "Вот события за "
                 (number-to-string часов-новостей)
                 " часа. "
                 (or промт промт-новости-по-умолчанию)
                 текст-новостей)
          nil))))))

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

(defun смешное-за-час ()
  "Рассказывает новости Elfeed за день."
  (interactive)
  (новости-за-время 1
                    "отфильтруй только смешные или просто несуразные и курьёзные новости и суммаризируй, простым текстом, без Markdown, только суммаризацию, но в стиле робота из Автостопом по галактике, добавь максимально ироничный и грустный комментарий робота:"))

(defun война-за-час ()
  "Рассказывает новости Elfeed за день."
  (interactive)
  (новости-за-время 1
                    "покажи только военные новости, разбей на группы по смыслу, систематизируй и суммаризируй, отфильтруй спам и рекламу, простым текстом, без Markdown:"))


(defun анализ-за-час ()
  "Рассказывает анализ новостей за час."
  (interactive)
  (новости-за-время 1
                    "отфильтруй спам и рекламу, и сделай анализ и предсказание по заголовкам новостей: напиши только результат анализа, простым текстом, без Markdown:"))

(provide 'про-искусственный-интеллект)

;;; про-искусственный-интеллект.el ends here

