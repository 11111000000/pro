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
  :defer t 
  :init (установить-из :repo "xenodium/chatgpt-shell")
  :bind (:map chatgpt-shell-mode-map
                ("C-c C-c" . chatgpt-shell-interrupt))
  :custom (
          ;; Настройка версии моделей и URL API
          ;; (по-умолчанию используется первая из списка, переключение в шелле C-c C-v)
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
          (dall-e-shell--url "https://api.proxyapi.ru/v1/images/generations")
          (chatgpt-shell-transmitted-context-length 30))
  :config
  ;; Поддержка блоков Org-мод
  ;; Пример:   #+begin_src chatgpt-shell :version "gpt-4o" :system "результат в формате org-mode" :context emacs

  )

(use-package ob-chatgpt-shell
  :defer t 
  :ensure t
  :functions ()
  :config
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup))


;;;; Поддержка локальной нейросети LLAMA

(use-package ellama
  :defer t 
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
  :defer t 
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
  :defer t 
  :init (установить-из :repo "natrys/whisper.el")
  :bind ("M-<f5>" . whisper-run)
  :custom ((whisper--ffmpeg-input-format ))
  :config
  (setq whisper-install-directory (expand-file-name "~/.emacs.d/")
       whisper-model "medium"
       whisper-language "ru"
       whisper-translate nil
       whisper-use-threads (/ (num-processors) 2)))


;;;; Обработка новостей

(require 'chatgpt-shell)

(defvar промт-новости-по-умолчанию "убери рекламу, систематизируй и кратко суммаризируй все новости, добавь аналитику на главные темы")

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

(defun куплет-за-сутки ()
  "Поёт новости Elfeed за сутки."
  (interactive)
  (новости-за-время 24
                    "Сперва сгруппируй новости на одну тему, а потом сделай лёгкий красивый поэтический рифмованый пересказ:"))


(defun смешное-за-час ()
  "Рассказывает новости Elfeed за день."
  (interactive)
  (новости-за-время 1
                    "Перескажи только смешные или просто несуразные и курьёзные новости и суммаризируй, простым текстом, без Markdown, только суммаризацию, неформальным, лёгким языком :"))

(defun смешное-за-сутки ()
  "Рассказывает смешные новости Elfeed за сутки."
  (interactive)
  (новости-за-время 24
                    "Перескажи только смешные или просто несуразные и курьёзные новости и суммаризируй, простым текстом, без Markdown, только суммаризацию, неформальным, лёгким языком :"))


(defun война-за-сутки ()
  "Рассказывает новости войны за сутки."
  (interactive)
  (новости-за-время 24
                    "Перескажи кратко только военные новости, разбей на группы по смыслу, систематизируй и суммаризируй, простым текстом, без Markdown:"))


(defun анализ-за-сутки ()
  "Рассказывает анализ новостей за сутки."
  (interactive)
  (новости-за-время 24
                    "убери рекламу, и сделай анализ и предсказание по заголовкам новостей: напиши только результат анализа, простым текстом, без Markdown:"))

(provide 'про-искусственный-интеллект)

;;; про-искусственный-интеллект.el ends here

