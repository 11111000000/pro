;;; про-ии.el --- Искусственный Интеллект -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)

;;; Commentary:
;; Это файл конфигурации для интеграции различных сервисов искусственного интеллекта
;; в Emacs.  Здесь настраиваются пакеты для работы с нейросетями, такими как GPT,
;; Codeium, Whisper и другими.

;;; Code:

;; Начальная настройка: импорт необходимых функций и пакетов
(require 'установить-из)  ;; Вспомогательная функция для установки пакетов из репозиториев
(require 'cape)           ;; Пакет для дополнения кода

;;;; Настройка OpenAI API ключа

;; Если установлены переменные proxyapi-key и proxyapi-url, используем их
;; для настройки ключей доступа к OpenAI API через прокси-сервер
(if (and (boundp 'proxyapi-key) (boundp 'proxyapi-url))
    (progn
      ;; Устанавливаем ключи API для различных пакетов, использующих OpenAI
      (setq-default openai-key proxyapi-key)
      (setq-default gptel-api-key proxyapi-key)
      (setq-default chatgpt-shell-openai-key proxyapi-key)
      (setq-default dall-e-shell-openai-key proxyapi-key)))

;;;; GPTEL - универсальный интерфейс для работы с ИИ

;; Настраиваем пакет gptel для общения с моделями GPT внутри Emacs
(use-package gptel
  :ensure t
  :functions (gptel-make-openai)
  :custom (
           (gptel-default-mode 'org-mode) ;; Устанавливаем режим по умолчанию для буферов gptel
           (gptel-org-branching-context t) ;; Включаем ветвление контекста в org-mode
           (gptel-api-key 'proxyapi-key) ;; Используем proxyapi-key в качестве ключа API
           (gptel-proxy "socks5h://localhost:9050") ;; Устанавливаем прокси-сервер
           (gptel-system-prompt "Ты - большая языковая модель, живущая внутри EMACS, а также специалист по Haskell, LISP и функциональному программированию. Отвечай кратко и ёмко. Ответ выдавай в org-mode, в src_block указывай :file, если известен путь к файлу.") ;; Системный промпт для модели GPT
           (gptel-backend (gptel-make-openai "ProxyAPI ChatGPT"
                            :protocol "https"
                            :host "api.proxyapi.ru"
                            :endpoint "/openai/v1/chat/completions"
                            :stream nil
                            :key gptel-api-key
                            :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
                            :models gptel--openai-models)) ;; Настраиваем бэкенд для работы через прокси API
           )
  :init)

;;;; GptEl-quick - быстрые запросы к GPT по точке

;; Пакет для быстрого выполнения запросов к GPT на основе текущего положения курсора
(use-package gptel-quick
  :after gptel
  :custom (
           (gptel-quick-model :o1-mini) ;; Модель по умолчанию для быстрых запросов
           (gptel-quick-backend gptel-backend) ;; Используем тот же бэкенд, что и для gptel
           )
  :init
  (установить-из :repo "karthink/gptel-quick"))

;;;; Elysium - управление окнами для отображения результатов ИИ

;; Настраиваем пакет Elysium для управления окнами в Emacs при работе с ИИ
(use-package elysium
  :ensure t
  :custom
  (elysium-window-size 0.33) ;; Устанавливаем размер окна для отображения результатов
  (elysium-window-style 'vertical) ;; Задаем стиль окна (вертикальный или горизонтальный)
  )

;;;; Evedel - среда разработки с использованием LLM (Large Language Models)

;; Настраиваем пакет evedel для интеграции LLM в процесс разработки
(use-package evedel
  :ensure t
  :custom (
           ;; Определяем роли для различных режимов редактирования
           (e-descriptive-mode-roles
            '((emacs-lisp-mode . "an Emacs Lisp programmer")
              (js-mode         . "a JavaScript programmer")
              (js-ts-mode      . "a JavaScript programmer")
              (typescript-ts-mode . "a TypeScript programmer")
              (typescript-mode . "a TypeScript programmer")
              (haskell-ts-mode . "a Haskell programmer")
              (bash-ts-mode    . "a Bash programmer")
              (c-mode          . "a C programmer")
              (c++-mode        . "a C++ programmer")
              (lisp-mode       . "a Common Lisp programmer")
              (web-mode        . "a web developer")
              (erlang-mode     . "an Erlang programmer")))))

;;;; ChatGPT Shell - REPL для различных нейросетей

;; Настраиваем пакет chatgpt-shell для интерактивного общения с моделями ChatGPT
(use-package chatgpt-shell
  :init (установить-из :repo "xenodium/chatgpt-shell")
  :bind (
         :map chatgpt-shell-mode-map
         ;; Привязываем сочетание клавиш C-g для прерывания текущего запроса
         ("C-g" . chatgpt-shell-interrupt))
  :custom (
           
           (chatgpt-shell-model-versions '("o1-mini"
                                           "o1-preview"
                                           "gpt-4o-mini"
                                           "gpt-4o"
                                           "gpt-4-turbo"
                                           "gpt-4"
                                           "gpt-3.5-turbo-0125"
                                           "gemini-1.5-pro"
                                           "gemini-1.5-flash"
                                           "claude-3-opus-20240229")) ;; Определяем список версий моделей для использования
           
           (chatgpt-shell-api-url-base  "https://api.proxyapi.ru/openai") ;; Устанавливаем базовый URL для API
           (dall-e-shell--url "https://api.proxyapi.ru/v1/images/generations") ;; URL для генерации изображений через DALL-E
           (chatgpt-shell-streaming nil) ;; Отключаем стриминг ответов
           (chatgpt-shell-transmitted-context-length 0) ;; Устанавливаем длину передаваемого контекста в 0 (без истории)
           (chatgpt-shell-system-prompt "") ;; Оставляем системный промпт пустым
           )
  :config)

;; Переопределяем функцию приветственного сообщения для shell-maker

(require 'shell-maker)

(defun shell-maker-welcome-message (config)
  "Return a welcome message to be printed using CONFIG."
  (format
   "Welcome to %s shell\n\n  Type %s and press %s for details.\n\n"
   (propertize (shell-maker-config-name config)
               'font-lock-face 'font-lock-comment-face)
   (propertize "help" 'font-lock-face 'italic)
   (shell-maker--propertize-key-binding "-shell-submit" config)))

;;;; Поддержка блоков Org-mode для интеграции с ChatGPT Shell

;; Настраиваем выполнение блоков кода в org-mode с использованием chatgpt-shell
;; Пример использования:
;; #+begin_src chatgpt-shell :version "gpt-4o" :system "результат в формате org-mode" :context emacs

(use-package ob-chatgpt-shell
  :ensure t
  :functions (ob-chatgpt-shell-setup)
  :config
  (require 'ob-chatgpt-shell)
  (ob-chatgpt-shell-setup)

  ;; Добавляем шаблон для быстрого вставки блока chatgpt-shell в org-mode
  (add-to-list 'org-structure-template-alist
               '("gpt" . "src chatgpt-shell :context nil :version \"gpt-4o-mini\" :system nil"))

  ;; Функция для преобразования блоков example в src markdown после выполнения
  (defun my/convert-example-to-src-markdown ()
    "Convert example blocks to src markdown blocks in results."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "#+begin_example" nil t)
        (replace-match "#+begin_src markdown"))
      (goto-char (point-min))
      (while (search-forward "#+end_example" nil t)
        (replace-match "#+end_src"))))
  
  ;; Добавляем хук для преобразования блоков после выполнения кода
  (add-hook 'org-babel-after-execute-hook 'my/convert-example-to-src-markdown))

;;;; Поддержка локальных нейросетей LLAMA

;; Настраиваем пакет ellama для работы с локальными моделями LLAMA
(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  
  (setopt ellama-language "Russian") ;; Устанавливаем язык по умолчанию на русский
  
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "codellama"
           :embedding-model "codellama")) ;; Настраиваем провайдера для использования модели codellama
  )

;;;; Дополнение кода с использованием нейросети Codeium

;; Настраиваем пакет codeium для автоматического дополнения кода
(use-package codeium
  :init (установить-из :repo "Exafunction/codeium.el")
  :functions (codeium-utf8-byte-length codeium-init codeium-completion-at-point)
  :bind
  ("C-c <tab>" . codeium-complete)
  :config
  
  ;; Функция для включения codeium
  (defun codeium-on ()
    "Enable codeium."
    (interactive)
    (codeium-init)
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))

  ;; Функция для отключения codeium
  (defun codeium-off ()
    "Disable codeium."
    (interactive)
    (setq completion-at-point-functions
          (remove #'codeium-completion-at-point completion-at-point-functions)))

  ;; Функция для переключения состояния codeium
  (defun codeium-toggle ()
    "Toggle codeium."
    (interactive)
    (if (memq #'codeium-completion-at-point completion-at-point-functions)
        (codeium-off)
      (codeium-on)))

  ;; Определяем alias для функции codeium-complete
  (defalias 'codeium-complete
    (cape-capf-interactive #'codeium-completion-at-point))

  ;; Отключаем использование диалоговых окон
  (setq use-dialog-box nil)

  ;; Настраиваем какие API доступны
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  ;; Можно добавить дополнительные настройки для конкретных буферов
  ;; Например, для python-mode:
  ;; (add-hook 'python-mode-hook
  ;;           (lambda ()
  ;;             (setq-local codeium/editor_options/tab_size 4)))
  )

;;;; Распознавание голоса с помощью Whisper

;; Настраиваем пакет whisper для преобразования речи в текст
;; Можно использовать для диктовки комментариев или кода
(use-package whisper
  :init (установить-из :repo "natrys/whisper.el")
  :bind
  :custom (
          (whisper--ffmpeg-input-format "alsa") ;; Настраиваем формат ввода для ffmpeg (используем Alsa)
          )
  :config
  
  (setq whisper-install-directory (expand-file-name "~/.emacs.d/") ;; Указываем директорию для установки моделей
        whisper-model "large" ;; Выбираем модель для распознавания (large обеспечивает лучшую точность)
        whisper-language "ru" ;; Устанавливаем язык распознавания на русский
        whisper-translate nil ;; Отключаем перевод (оставляем язык оригинала)
        whisper-quantize nil ;; Не используем квантизацию для улучшения качества
        whisper-insert-text-at-point t ;; Вставляем распознанный текст в точку курсора
        whisper-recording-timeout 3600  ;; Задаем максимальное время записи
        whisper-use-threads (/ (num-processors) 1) ;; Используем все доступные процессоры для ускорения обработки
        ))

(provide 'про-ии)

;;; про-ии.el ends here
