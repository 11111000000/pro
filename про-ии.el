;;; про-ии.el --- Интеграция искусственного интеллекта в Emacs -*- lexical-binding: t; -*-

;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: AI, GPT, Codeium, Whisper, ChatGPT
;; URL: https://example.com/про-ии

;;; Commentary:
;; Этот конфигурационный файл настраивает интеграцию различных сервисов искусственного интеллекта
;; в Emacs.  Здесь подключаются пакеты для работы с нейросетями (GPT, Codeium, Whisper и др.),
;; а также настраиваются API ключи и параметры для диалога с моделями.
;; 
;; Перед использованием убедитесь, что переменная `proxyapi-key` определена.
;; 
;; Чтобы применить данный конфиг, загрузите этот файл в Emacs, например:
;;   M-x load-file RET путь/до/про-ии.el RET

;;; Code:

;; ---------- Кастомизируемая таблица цен за токены ----------
(defcustom pro-gptel-model-pricing-alist
  '(
    ;; --- Anthropic ---
    (claude-sonnet-4-20250514         :input 0.7344   :output 3.672)
    (claude-opus-4-20250514           :input 3.672    :output 18.36)
    (claude-3-sonnet-20240229         :input 0.7344   :output 3.672)
    (claude-3-opus-20240229           :input 3.672    :output 18.36)
    (claude-3-haiku-20240307          :input 0.0612   :output 0.306)
    (claude-3-7-sonnet-20250219       :input 0.7344   :output 3.672)
    (claude-3-5-sonnet-20241022       :input 0.7344   :output 3.672)
    (claude-3-5-sonnet-20240620       :input 0.7344   :output 3.672)
    (claude-3-5-haiku-20241022        :input 0.2448   :output 1.224)

    ;; --- DeepSeek ---
    (deepseek-reasoner                :input 0.13464  :output 0.53611)
    (deepseek-chat                    :input 0.06610  :output 0.26928)

    ;; --- Google Gemini ---
    (gemini-2.5-pro-preview-06-05     :input 0.306    :output 2.448)
    (gemini-2.5-pro-preview-06-05-200k :input 0.612   :output 3.672)
    (gemini-2.5-pro-preview-05-06     :input 0.306    :output 2.448)
    (gemini-2.5-pro-preview-05-06-200k :input 0.612   :output 3.672)
    (gemini-2.5-pro-preview-03-25     :input 0.306    :output 2.448)
    (gemini-2.5-pro-preview-03-25-200k :input 0.612   :output 3.672)
    (gemini-2.5-pro                   :input 0.306    :output 2.448)
    (gemini-2.5-pro-200k              :input 0.612    :output 3.672)
    (gemini-2.5-flash-preview-05-20   :input 0.03672  :output 0.14688)
    (gemini-2.5-flash-preview-05-20-audio :input 0.2448 :output 0.8568) ; размышления — output
    (gemini-2.5-flash-preview-04-17   :input 0.03672  :output 0.14688)
    (gemini-2.5-flash-preview-04-17-audio :input 0.2448 :output 0.8568)
    (gemini-2.5-flash-lite-preview-06-17 :input 0.02448 :output 0.1224)
    (gemini-2.5-flash-lite-preview-06-17-audio :input 0.09792 :output 0.1224)
    (gemini-2.5-flash                 :input 0.07344  :output 0.612)
    (gemini-2.5-flash-audio           :input 0.2448   :output 0.612)
    (gemini-2.0-flash-lite            :input 0.01836  :output 0.07344)
    (gemini-2.0-flash                 :input 0.02448  :output 0.09792)
    (gemini-2.0-flash-audio           :input 0.17136  :output 0.09792)
    (gemini-1.5-pro                   :input 0.8568   :output 1.7136)
    (gemini-1.5-pro-128k              :input 2.5704   :output 5.1408)
    (gemini-1.5-flash                 :input 0.01836  :output 0.07344)

    ;; --- OpenAI ---
    (o4-mini-2025-04-16               :input 0.26928  :output 1.07712)
    (o3-pro-2025-06-10                :input 2.4      :output 9.6)
    (o3-mini-2025-01-31               :input 0.26928  :output 1.07712)
    (o3-2025-04-16                    :input 0.576    :output 1.6)
    (o1-pro-2025-03-19                :input 15.3     :output 76.5)
    (o1-preview-2024-09-12            :input 2.55     :output 7.65)
    (o1-mini-2024-09-12               :input 0.7344   :output 1.53)
    (o1-2024-12-17                    :input 2.55     :output 7.65)
    (gpt-4o-search-preview-2025-03-11             :input 0.612    :output 2.448)
    (gpt-4o-mini-search-preview-2025-03-11        :input 0.03672  :output 0.14688)
    (gpt-4o-mini-audio-preview-2024-12-17-audio   :input 2.448    :output 4.896)
    (gpt-4o-mini-2024-07-18                       :input 0.03672  :output 0.14688)
    (gpt-4o-audio-preview-2024-12-17-audio        :input 24.48    :output 48.96)
    (gpt-4o-audio-preview-2024-10-01-audio        :input 24.48    :output 48.96)
    (gpt-4o-64k-output-alpha                      :input 1.4688   :output 4.4064)
    (gpt-4o-2024-11-20                            :input 0.612    :output 2.448)
    (gpt-4o-2024-08-06                            :input 0.612    :output 2.448)
    (gpt-4o-2024-05-13                            :input 1.224    :output 3.672)
    (gpt-4.5-preview-2025-02-27                   :input 8.5      :output 25.5)
    (gpt-4.1-nano-2025-04-14                      :input 0.02448  :output 0.09792)
    (gpt-4.1-mini-2025-04-14                      :input 0.09792  :output 0.39168)
    (gpt-4.1-2025-04-14                           :input 0.4896   :output 1.9584)
    (gpt-4-turbo-2024-04-09                       :input 2.448    :output 7.344)
    (gpt-4-32k-0613                               :input 14.688   :output 29.376)
    (gpt-4-1106-preview                           :input 2.448    :output 7.344)
    (gpt-4-0613                                   :input 7.191    :output 14.688)
    (gpt-4-0125-preview                           :input 2.448    :output 7.344)
    (gpt-3.5-turbo-16k-0613                       :input 0.731    :output 0.9775)
    (gpt-3.5-turbo-1106                           :input 0.255    :output 0.51)
    (gpt-3.5-turbo-0613                           :input 0.3655   :output 0.493)
    (gpt-3.5-turbo-0125                           :input 0.1224   :output 0.3672)
    (computer-use-preview-2025-03-11              :input 0.51     :output 1.224)
    (codex-mini-latest                            :input 0.26928  :output 1.07712)

    ;; --- Генерация изображений (пример записи, если требуется) ---
    ;; (gpt-image-1-medium-1024x1024 :input 10.2)
    ;; (gpt-image-1-medium-1024x1536 :input 15.3)
    ;; (gpt-image-1-low-1024x1024    :input 2.55)
    ;; (dall-e-3 :input 9.79)
    ;; --- ТТС и прочее (пример, если захотите учесть для estimate) ---
    ;; (tts-1-hd :input 7.344)
    ;; (whisper-1 :input 1.47)
    )
  "Alist: (MODEL . (:input RUB/1k-tokens :output RUB/1k-tokens)), цены за токены для расходов в рублях.
Значения указаны для 1000 токенов. Можно настраивать через customize."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type number))
  :group 'gptel)

(defcustom pro-gptel-currency-rate 1.0
  "Курс пересчёта если тарифы указаны не в рублях. Обычно 1.0 (=рубли).
Если указаны в долларах, можно поставить тут курс."
  :type 'number
  :group 'gptel)

(defvar-local pro-gptel-last-cost-rub nil
  "Последняя рассчитанная стоимость токенов для gptel-mode, в рублях (float).
Используется для отображения в header-line.")

(defun pro-gptel-model-pricing (model)
  "Вернуть plist вида (:input ... :output ...) для модели MODEL.
Если точного совпадения нет, ищет модель по началу строки (префиксу)."
  (let* ((name (if (symbolp model) (symbol-name model) model)))
    (or
     (cdr (assoc (intern name) pro-gptel-model-pricing-alist))
     (let ((match (seq-find (lambda (x)
                              (string-prefix-p name (symbol-name (car x))))
                            pro-gptel-model-pricing-alist)))
       (and match (cdr match))))))

(defun pro-gptel-estimate-cost (n-in n-out model)
  "Прикинуть стоимость по количеству токенов N-IN (prompt), N-OUT (response) и модели MODEL.
Возвращает float рублей (с учётом pro-gptel-currency-rate)."
  (let* ((pricing (pro-gptel-model-pricing model))
         (in (or (plist-get pricing :input) 0))
         (out (or (plist-get pricing :output) 0)))
    (* pro-gptel-currency-rate
       (+ (* (/ (float n-in) 1000) in)
          (* (/ (float n-out) 1000) out)))))

;;;; Подключение трекинга стоимости токенов для gptel-mode

(defun pro-gptel--tokens-from-info (start end &optional model)
  "Точный подсчет токенов: вызывает tiktoken через внешний скрипт.
Если не найден скрипт ~/bin/tiktoken-counter.py — возвращает оценку по символам."
  (let* ((text (buffer-substring-no-properties start end))
         (tmpfile (make-temp-file "tik-tok-txt-"))
         (script (or (executable-find "tiktoken-counter.py")
                     (expand-file-name "~/.local/bin/tiktoken-counter.py")))
         (model (or model "gpt-4"))
         tokens)
    (with-temp-file tmpfile (insert text))
    (if (and (file-exists-p script) (file-executable-p script))
        (setq tokens
              (string-to-number
               (with-temp-buffer
                 (call-process script tmpfile t nil model)
                 (buffer-string))))
      ;; fallback: по символам
      (setq tokens (round (/ (length text) 3.5))))
    (delete-file tmpfile)
    (max 1 tokens)))

(defun pro-gptel-post-response-cost (beg end)
  "Вычислить стоимость последнего ответа GPT и сохранить её в `pro-gptel-last-cost-rub`.
Хук для `gptel-post-response-functions`, срабатывает только в `gptel-mode`.
Показывает стоимость в echo area и *Messages*."
  (when (and (bound-and-true-p gptel-mode) (> end beg))
    (let* ((buf (current-buffer))
           (response-len (pro-gptel--tokens-from-info beg end))
           (prompt-tokens (pro-gptel--tokens-from-info (point-min) beg))
           (model (if (boundp 'gptel-model) gptel-model 'unknown))
           (cost (pro-gptel-estimate-cost prompt-tokens response-len model)))
      (setq-local pro-gptel-last-cost-rub cost)
      (message "Стоимость запроса: ≈%.2f₽ (модель: %s, prompt: %d токенов, response: %d токенов)"
               cost model prompt-tokens response-len))))

(add-hook 'gptel-post-response-functions #'pro-gptel-post-response-cost)

;; Теперь стоимость выводится только в *Messages*

;; Импорт необходимых модулей
(require 'установить-из)  ;; Функция для установки пакетов из репозиториев
(require 'cape)           ;; Пакет для автодополнения

;;;; Настройка API ключей

(defvar tunnelai-url)
(defvar tunnelai-key)
(defvar tunnelai-backend)
(defvar chutes-api-key)
(defvar proxyapi-url)
(defvar proxyapi-key)

;; Если переменная proxyapi-key определена, используем её для настройки ключей API
(when (boundp 'proxyapi-key)
  (setq-default openai-key proxyapi-key)
  (setq-default gptel-api-key proxyapi-key)
  (setq-default chatgpt-shell-openai-key proxyapi-key)
  (setq-default dall-e-shell-openai-key proxyapi-key)
  (setenv "OPENAI_API_KEY" proxyapi-key))

;;;; Настройка пакета GPTEL

;; Настройка GPT-сервисов

(use-package gptel
  :ensure t
  :functions (gptel-make-openai gptel--get-api-key gptel-aibo-apply-last-suggestions)
  :bind (:map gptel-mode-map
              ("C-c RET"      . gptel-send)
              ("C-c C-<return>"      . gptel-send)
         ("M-RET"        . pro/gptel-send-no-context)
         ("C-c M-RET"    . pro/gptel-aibo-no-context))
  :custom
  ((gptel-default-mode 'org-mode)                ;; Режим по умолчанию для gptel
   (gptel-org-branching-context nil)             ;; Отключить ветвление контекста в org-mode
   (gptel-api-key proxyapi-key)                  ;; Ключ API берётся из proxyapi-key
   (gptel-log-level 'info)
   (gptel--system-message
    "Ты — большая языковая модель, живущая в Emacs под Linux Debian bookworm. Отвечай в виде Org-mode."))
  :config
  ;; Реализация отправки в aibo без контекста: C-c M-RET
  (defun pro/gptel-aibo-no-context ()
    "Отправить в aibo без контекста (gptel-use-context = nil, через gptel-aibo-send)."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-aibo-send)))
  
  (defun pro/gptel-send-no-context ()
    "Отправить в aibo без контекста (gptel-use-context = nil, через gptel-aibo-send)."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-send)))
  ;; Подключаем библиотеку gptel-context-store из .libs рядом с текущим конфигом
  (require 'gptel-context-store)

  ;; Создаем несколько бэкендов для gptel
  (setq tunnelai-backend (gptel-make-openai "TunnelAI"
                           :protocol "https"
                           :host tunnelai-url
                           :endpoint "/v1/chat/completions"
                           :stream t
                           :key tunnelai-key
                           :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
                           :models (append '("gpt-4.5-preview"
                                             "gpt-4.1"
                                             "gpt-4.1-mini"
                                             "gpt-4.1-nano"
                                             "o3"
                                             "o3-mini"
                                             "o1-pro"
                                             "o1"
                                             "o1-mini"
                                             "o4-mini"
                                             "gpt-4o-search-preview"
                                             "gpt-4o-mini-search-preview"
                                             "gpt-4o-audio-preview"
                                             "gemini-2.5-pro-preview-03-25"
                                             "gemini-2.5-flash-preview-05-20"
                                             "gemini-2.5-flash-preview-05-20-thinking"
                                             "gpt-4.5-preview"
                                             "deepseek-r1"
                                             "deepseek-chat"
                                             "grok-3-mini-beta"
                                             "grok-3-beta"
                                             )
                                           gptel--openai-models)))

  (gptel-make-openai "Proxy OpenAI"
    :protocol "https"
    :host "api.proxyapi.ru"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models (append '("gpt-4o-search-preview"
                      "gpt-4o-mini-search-preview"
                     "gpt-4.1"
                     "gpt-4.1-mini"
                     "gpt-4.1-nano"
                     "o3"
                     "o3-mini"
                     "o4-mini"
                     "o4-mini-high"
                     "gpt-4.5-preview"
                     "o1"
                     "o1-mini"
                     "o1-pro"
                     "dall-e-3"
                     "gpt-4o"
                     "gpt-4o-mini"
                     "gpt-4o-audio-preview"
                     "gpt-4o-mini-audio-preview"
                     "computer-use-preview") gptel--openai-models))

  (gptel-make-openai "ProxyAPI Anthropic"
    :protocol "https"
    :host "api.proxyapi.ru"
    :endpoint "/anthropic/v1/messages"
    :stream nil
    :key gptel-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("claude-3-5-sonnet-20241022" "claude-3-opus-20240229"))

  (gptel-make-openai "Chutes"
    :protocol "https"
    :host "llm.chutes.ai"
    :endpoint "/v1/chat/completions"
    :stream nil
    :key chutes-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("deepseek-ai/DeepSeek-V3-0324"))

  (setq gptel-backend (gptel-get-backend "Proxy OpenAI"))
  (setq gptel-model 'gpt-4.1))

(use-package gptel-aibo
  :ensure t
  :bind (:map gptel-aibo-mode-map
              ("C-c RET"      . gptel-aibo-send)
              ("C-c C-<return>"      . gptel-aibo-send)
              ("C-c M-RET"    . pro/gptel-aibo-no-context)))

;;;; Настройка gptel-quick для быстрых запросов

(use-package gptel-quick
  :after gptel
  :init
  (установить-из :repo "karthink/gptel-quick")
  (setq gptel-quick-backend gptel-backend)
  (setq gptel-quick-model 'gpt-4.1))


;; ;;;; Настройка Elysium (WTF)

;; (use-package elysium
;;   :ensure t
;;   :custom
;;   (elysium-window-size 0.33)
;;   (elysium-window-style 'vertical))

;;;; Настройка Minuet для автодополнения с GPT

(use-package minuet
  :ensure t
  :defines (minuet-active-mode-map
            minuet-provider
            minuet-openai-fim-compatible-options
            minuet-auto-suggestion-mode
            minuet-openai-compatible-options
            minuet-auto-suggestion-throttle-delay
            minuet-auto-suggestion-debounce-delay
            minuet-request-timeout)
  :functions (minuet-complete-with-minibuffer
              minuet-auto-suggestion-mode
              minuet-show-suggestion
              minuet-configure-provider
              minuet-previous-suggestion
              minuet-next-suggestion
              minuet-accept-suggestion
              minuet-accept-suggestion-line
              minuet-dismiss-suggestion
              minuet-set-optional-options)
  :bind (("s-i y" . minuet-complete-with-minibuffer)
         ("s-i TAB" . minuet-show-suggestion)
         ("s-i s-<tab>" . minuet-complete-with-minibuffer)
         ("s-i s-m" . minuet-configure-provider)
         :map minuet-active-mode-map
         ("M-p" . minuet-previous-suggestion)
         ("M-n" . minuet-next-suggestion)
         ("RET" . minuet-accept-suggestion)
         ("C-RET" . minuet-accept-suggestion)
         ("M-RET" . minuet-accept-suggestion-line)
         ("C-g" . minuet-dismiss-suggestion)
         ("M-q" . minuet-dismiss-suggestion)
         ("s-q" . minuet-dismiss-suggestion)
         ("M-a" . minuet-accept-suggestion-line))
  :custom ((minuet-context-window 16000)
           (minuet-request-timeout 3)
           (minuet-context-ratio 0.75))
  :config

  (plist-put minuet-openai-options :model "gpt-4-turbo")
  (setopt minuet-provider 'openai-compatible)
  (minuet-auto-suggestion-mode -1)
  (plist-put minuet-openai-compatible-options :end-point (concat "https://" proxyapi-url "/openai/v1/chat/completions"))
  (plist-put minuet-openai-compatible-options :api-key "OPENAI_API_KEY")
  
  (plist-put minuet-openai-compatible-options :model "gpt-4-turbo")
  (minuet-set-optional-options minuet-openai-compatible-options :provider nil)
  (minuet-set-optional-options minuet-openai-compatible-options :max_tokens nil)
  (minuet-set-optional-options minuet-openai-compatible-options :max_completion_tokens 512)
  (minuet-set-optional-options minuet-openai-compatible-options :top_p nil))

;;;; Настройка пакета Evedel для интеграции LLM в процесс разработки

(use-package evedel
  :ensure t
  :custom
  ((evedel-empty-tag-query-matches-all nil)
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

;;;; Инструмент gptel-tool: выполнение git-команд в текущем проекте

(defun pro/gptel-git-command (command)
  "Выполнить git-команду COMMAND в корне текущего проекта.
Возвращает stdout+stderr для передачи нейросети."
  (let ((project-root
         (or (and (fboundp 'project-current)
                  (cdr (project-current)))
             default-directory)))
    (with-temp-buffer
      (let ((default-directory project-root))
        (call-process-shell-command (concat "git " command)
                                   nil (current-buffer) t))
      (buffer-string))))

;; (with-eval-after-load 'gptel
  ;; (gptel-make-tool
  ;;  :function #'pro/gptel-git-command
  ;;  :name "run_git_command"
  ;;  :description "Run any git command in the root of the current project. The argument is a valid git command, e.g. 'status', 'log --oneline', or 'diff HEAD~1'. Returns the raw output of git (stdout and stderr)."
  ;;  :args (list
  ;;         (list :name "command"
  ;;               :type 'string
  ;;               :description "A valid git command to run, e.g. 'status', 'log --oneline', or 'diff HEAD~1'.")))

   ;; (add-to-list 'gptel-tools (gptel-get-tool "run_git_command")))

;;;; Настройка chatgpt-shell для REPL с ChatGPT

(use-package chatgpt-shell
  :init (установить-из :repo "xenodium/chatgpt-shell")
  :bind (:map chatgpt-shell-mode-map
              ("C-g" . chatgpt-shell-interrupt))
  :custom ((chatgpt-shell-model-versions
           '("o3-mini" "o1-mini" "o1" "gpt-4o-mini" "o3"
              "gpt-4o" "gpt-4-turbo" "gpt-4" "gpt-3.5-turbo-0125" "dall-e-3"
              "gemini-1.5-pro" "gemini-1.5-flash" "claude-3-opus-20240229"))
           (chatgpt-shell-api-url-base "https://api.proxyapi.ru/openai")
           (chatgpt-shell-anthropic-api-url-base "https://api.proxyapi.ru/anthropic")
           (dall-e-shell--url "https://api.proxyapi.ru/openai/v1/images/generations")
           (chatgpt-shell-streaming nil)
           (chatgpt-shell-transmitted-context-length 0)
           (chatgpt-shell-system-prompt ""))
  :config)

;;;; Настройка ob-chatgpt-shell для Org Babel

(use-package ob-chatgpt-shell
  :ensure t
  :functions (ob-chatgpt-shell-setup)
  :config
  (ob-chatgpt-shell-setup)
  (add-to-list 'org-structure-template-alist
               '("gpt" . "src chatgpt-shell :context nil :version \"gpt-4o-mini\" :system nil"))
  (defun my/convert-example-to-src-markdown ()
    "Преобразовать блоки примеров в блоки исходного кода markdown в результатах."
    (save-excursion
      (goto-char (point-min))
      (while (search-forward "#+begin_example" nil t)
        (replace-match "#+begin_src markdown"))
      (goto-char (point-min))
      (while (search-forward "#+end_example" nil t)
        (replace-match "#+end_src"))))
  (add-hook 'org-babel-after-execute-hook 'my/convert-example-to-src-markdown))

;;;; Настройка поддержки LLAMA для локальных моделей

(use-package ellama
  :ensure t
  :init
  (require 'llm-ollama)
  (setopt ellama-language "Russian")
  (setopt ellama-provider
          (make-llm-ollama :chat-model "codellama"
                           :embedding-model "codellama")))

;;;; Настройка Codeium для автодополнения кода

(use-package codeium
  :init (установить-из :repo "Exafunction/codeium.el")
  :functions (codeium-utf8-byte-length codeium-init codeium-completion-at-point)
  :bind (("C-c <tab>" . codeium-complete))
  :config
  (defun codeium-on ()
    "Включить Codeium."
    (interactive)
    (codeium-init)
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))
  (defun codeium-off ()
    "Отключить Codeium."
    (interactive)
    (setq completion-at-point-functions
          (remove #'codeium-completion-at-point completion-at-point-functions)))
  (defun codeium-toggle ()
    "Переключить состояние Codeium."
    (interactive)
    (if (memq #'codeium-completion-at-point completion-at-point-functions)
        (codeium-off)
      (codeium-on)))
  (defalias 'codeium-complete (cape-capf-interactive #'codeium-completion-at-point))
  (setq use-dialog-box nil)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion)))))

;;;; Настройка Whisper для распознавания речи

(use-package whisper
  :init (установить-из :repo "natrys/whisper.el")
  :custom ((whisper--ffmpeg-input-format "alsa"))
  :config
  (setq whisper-install-directory (expand-file-name "~/.emacs.d/"))
  (setq whisper-model "large")
  (setq whisper-language "ru")
  (setq whisper-translate nil)
  (setq whisper-quantize nil)
  (setq whisper-insert-text-at-point t)
  (setq whisper-recording-timeout 500)
  (setq whisper-use-threads (/ (num-processors) 1)))

;;;; Настройка Aidermacs для поддержки AI в редакторе

(use-package aidermacs
  :ensure t
  :config (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode nil)
  (aidermacs-show-diff-after-change nil)
  (setq aidermacs-auto-commits nil))

;;-------------------------------------------------------------
;; Выбор модели GPTEL из списка доступных
(defun gptel-set-model ()
  "Выбрать текущую модель для gptel в текущем буфере с помощью автодополнения.
Все модели из текущего `gptel-backend`."
  (interactive)
  (require 'gptel)
  (let* ((backend (if (boundp 'gptel-backend) gptel-backend (default-value 'gptel-backend)))
         (models (and backend (gptel-backend-models backend)))
         (model-names (mapcar #'gptel--model-name models))
         (current (if (symbolp gptel-model) (symbol-name gptel-model) (format "%s" gptel-model)))
         (choice (completing-read
                  (format "Модель [%s]: " current)
                  model-names nil t nil nil current)))
    (when choice
      (setq-local gptel-model (intern choice)))
    (message "gptel-model: %s" gptel-model)))

(provide 'про-ии)

;;; про-ии.el ends here
