;;; про-ии-core.el --- Ядро AI-интеграции ПРО -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Ядро AI-интеграции: GPTEL, тарифы, OpenRouter и подсчёт стоимости.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'url)
(require 'json)
(eval-when-compile (require 'cl-lib))

;; Public config group for all AI options.
(defgroup про-ии nil
  "Глубокая интеграция AI-сервисов в Emacs на базе GPTEL."
  :group 'applications
  :prefix "pro-ai-gptel-")

(defcustom pro-ai-gptel-org-файл-цен
  (expand-file-name "~/pro/тарифы-моделей.org")
  "Путь к org-файлу с таблицей цен за 1000 токенов (в рублях)."
  :type 'file
  :group 'про-ии)

(defcustom pro-ai-тарифы-моделей
  nil
  "Alist с ценами за токены в рублях."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type number))
  :group 'про-ии)

(defcustom pro-ai-gptel-курс-валюты 1.0
  "Курс пересчёта тарифов, если прайс не в рублях."
  :type 'number
  :group 'про-ии)

(defvar-local pro-ai-gptel-последняя-стоимость-руб nil
  "Последняя оцененная стоимость запроса в рублях.")

(defcustom pro-ai-gptel-openrouter-models-url "https://openrouter.ai/api/v1/models"
  "Endpoint for the current OpenRouter model catalog."
  :type 'string
  :group 'про-ии)

(defcustom pro-ai-gptel-openrouter-preferred-manufacturers '(qwen openai google nvidia minimax z-ai arcee-ai liquid cognitivecomputations meta-llama nousresearch)
  "Preferred OpenRouter vendors ordered by usefulness."
  :type '(repeat symbol)
  :group 'про-ии)

(defcustom pro-ai-gptel-openrouter-api-key nil
  "Optional OpenRouter API key for catalog requests."
  :type 'string
  :group 'про-ии)

(defvar pro-ai-gptel-openrouter-free-models-cache nil
  "Cached OpenRouter free model ids for this session.")

(defvar pro-ai-gptel-openrouter-backend-name "Openrouter"
  "Backend name used for the OpenRouter catalog.")

(defvar pro-ai-gptel-openrouter-preferred-models
  '(
    "qwen/qwen3-coder:free"
    "qwen/qwen3-next-80b-a3b-instruct:free"
    "google/gemma-4-26b-a4b-it:free"
    "google/gemma-4-31b-it:free"
    "nvidia/nemotron-3-super-120b-a12b:free"
    "openai/gpt-oss-120b:free"
    "minimax/minimax-m2.5:free"
    "meta-llama/llama-3.3-70b-instruct:free"
    "z-ai/glm-4.5-air:free")
  "OpenRouter free models sorted by usefulness (Apr 2026).")

(defvar pro-ai-gptel-openrouter-all-free-models
  '(
    ;; Qwen
    "qwen/qwen3-coder:free"
    "qwen/qwen3-next-80b-a3b-instruct:free"
    "qwen/qwen3.6-plus:free"
    "qwen/qwen3.6-plus-preview:free"
    "qwen/qwen3.5-coder:free"
    ;; Google
    "google/gemma-4-31b-it:free"
    "google/gemma-4-26b-a4b-it:free"
    ;; NVIDIA
    "nvidia/nemotron-3-super-120b-a12b:free"
    "nvidia/nemotron-nano-30b-a3b:free"
    "nvidia/nemotron-nano-12b-v2-vl:free"
    "nvidia/nemotron-nano-9b-v2:free"
    ;; OpenAI
    "openai/gpt-oss-120b:free"
    "openai/gpt-oss-20b:free"
    ;; MiniMax
    "minimax/minimax-m2.7:free"
    "minimax/minimax-m2.5:free"
    ;; Meta
    "meta-llama/llama-3.3-70b-instruct:free"
    "meta-llama/llama-3.2-3b-instruct:free"
    ;; Z AI
    "z-ai/glm-4.5-air:free")
  "All OpenRouter free models (Apr 2026).")

(defvar pro-ai-gptel-aitunnel-preferred-models
  '(
    ;; Light - бюджетные
    "qwen3-5-9b"
    "deepseek-chat-v3-1"
    "gpt-5-nano"
    ;; Balance - средние
    "qwen3-coder"
    "deepseek-v3-2"
    "gpt-5"
    ;; Strong - мощные
    "qwen3-max"
    "deepseek-r1"
    "gpt-5-pro")
  "AITunnel models: light/balance/strong by purpose.")

(defvar pro-ai-gptel-aitunnel-all-models
  '(
    ;; Qwen - light/balance/strong
    "qwen3-5-9b"
    "qwen3-coder"
    "qwen3-max"
    ;; DeepSeek
    "deepseek-chat-v3-1"
    "deepseek-v3-2"
    "deepseek-r1"
    ;; OpenAI
    "gpt-5-nano"
    "gpt-5"
    "gpt-5-pro"
    ;; Claude
    "claude-3-5-haiku"
    "claude-sonnet-4-6"
    "claude-opus-4-6"
    ;; Google
    "gemini-2-5-flash-lite"
    "gemini-2-5-flash"
    "gemini-2-5-pro"
    ;; Grok
    "grok-4-fast"
    "grok-code-fast-1"
    "grok-4"
    ;; MiniMax
    "minimax-m2"
    "minimax-m2-5"
    "minimax-m2-7"
    ;; Mistral
    "mistral-nemo"
    "mistral-small-2603"
    "mistral-large-2512"
    ;; Llama
    "llama-3-2-3b-instruct"
    "llama-3-2-11b-vision-instruct"
    "llama-4-maverick"
    ;; GLM
    "glm-4-5-air"
    "glm-4-7-flash"
    "glm-5"
    ;; Kimi
    "kimi-k2-0905"
    "kimi-k2-thinking"
    "kimi-k2-5"
    ;; Sonar
    "sonar"
    "sonar-pro"
    "sonar-pro-search"
    ;; Gigachat
    "gigachat-2"
    "gigachat-2-max"
    "gigachat-2-pro"
    ;; Gemma
    "gemma-4-26b-a4b-it"
    "gemma-4-31b-it")
  "All AITunnel available models (Apr 2026).")

(defvar pro-ai-gptel-qwen-full-line
  '(
    ;; Qwen 3 line (latest)
    "qwen3-coder-next"
    "qwen3-coder"
    "qwen3-max-thinking"
    "qwen3-max"
    "qwen3-6-plus"
    "qwen3-6-plus-preview"
    ;; Qwen 3.5
    "qwen3-5-397b-a17b"
    "qwen3-5-235b-a22b-2507"
    "qwen3-5-122b-a10b"
    "qwen3-5-35b-a3b"
    "qwen3-5-27b"
    "qwen3-5-9b"
    "qwen3-5-72b-a3b-instruct"
    "qwen3-5-32b-a3b-instruct"
    "qwen3-5-27b-a3b-instruct"
    "qwen3-5-coder"
    "qwen3-5-coder-instruct"
    "qwen3-5-math"
    "qwen3-5-omni"
    "qwen3-5-vl"
    ;; Qwen 2.5
    "qwen2-5-coder"
    "qwen2-5-coder-32b"
    "qwen2-5-coder-7b"
    "qwen2-5-math"
    "qwen2-5-math-72b"
    "qwen2-5-math-7b"
    "qwen2-5"
    "qwen2-5vl"
    "qwen2-5vl-32b"
    "qwen2-5vl-7b"
    ;; Qwen 2
    "qwen2"
    "qwen2-57b-a14b"
    "qwen2-72b"
    "qwen2-7b"
    ;; Qwen Plus
    "qwen-plus"
    "qwen-plus-32k"
    "qwen-turbo"
    "qwen-turbo-32k")
  "Full Qwen model line (Apr 2026).")

(defun pro-ai-gptel--openrouter-free-model-p (entry)
  "Return non-nil when ENTRY is a free OpenRouter model."
  (let* ((id (alist-get 'id entry))
         (pricing (alist-get 'pricing entry))
         (prompt (and pricing (alist-get 'prompt pricing)))
         (completion (and pricing (alist-get 'completion pricing))))
    (and (stringp id)
         (string-suffix-p ":free" id)
         (or (null pricing)
             (and (<= (string-to-number (format "%s" (or prompt 0))) 0)
                  (<= (string-to-number (format "%s" (or completion 0))) 0))))))

(defun pro-ai-gptel--preferred-model-in-list (models preferred)
  "Return the first PREFERRED model present in MODELS."
  (seq-find (lambda (name) (member name models)) preferred))

(defun pro-ai-gptel--aitunnel-preferred-models (models)
  "Return preferred AITunnel models from MODELS."
  (seq-filter (lambda (name) (member name models)) pro-ai-gptel-aitunnel-preferred-models))

(defun pro-ai-gptel--openrouter-fetch-models-json ()
  "Fetch the raw OpenRouter model catalog JSON."
  (let ((url-request-extra-headers
         (delq nil
               `(("Accept" . "application/json")
                 ,@(when (and pro-ai-gptel-openrouter-api-key
                              (not (string-empty-p pro-ai-gptel-openrouter-api-key)))
                     `(("Authorization" . ,(concat "Bearer " pro-ai-gptel-openrouter-api-key))))))))
    (condition-case nil
        (let ((buf (url-retrieve-synchronously pro-ai-gptel-openrouter-models-url t t 15)))
          (when (bufferp buf)
            (unwind-protect
                (with-current-buffer buf
                  (goto-char (point-min))
                  (when (search-forward "\n\n" nil t)
                    (let ((json-object-type 'alist)
                          (json-array-type 'list)
                          (json-key-type 'symbol)
                          (json-false nil))
                      (json-read))))
              (kill-buffer buf))))
      (error nil))))

(defun pro-ai-gptel-openrouter-free-models (&optional refresh)
  "Return a list of current OpenRouter free model ids.
If REFRESH is non-nil, bypass the session cache."
  (when (or refresh (null pro-ai-gptel-openrouter-free-models-cache))
    (setq pro-ai-gptel-openrouter-free-models-cache
          (let* ((payload (pro-ai-gptel--openrouter-fetch-models-json))
                 (items (alist-get 'data payload))
                 (free-items (seq-filter #'pro-ai-gptel--openrouter-free-model-p items)))
            (seq-uniq
             (delq nil (mapcar (lambda (entry) (alist-get 'id entry)) free-items))
             #'string=))))
  pro-ai-gptel-openrouter-free-models-cache)

(defun pro-ai-gptel--openrouter-set-backend (models)
  "Replace the OpenRouter backend with MODELS."
  (when про-ии--gptel-available
    (setq gptel--known-backends
          (assq-delete-all pro-ai-gptel-openrouter-backend-name gptel--known-backends))
    (pro/ai--register-backend
     pro-ai-gptel-openrouter-backend-name "openrouter.ai" "/v1/chat/completions" t
     models)
    (setq gptel-backend (gptel-get-backend pro-ai-gptel-openrouter-backend-name))
    (let ((backend-models (and gptel-backend (gptel-backend-models gptel-backend))))
      (setq gptel-model
            (or (let ((choice (pro-ai-gptel--preferred-model-in-list
                               (mapcar #'gptel--model-name backend-models)
                               pro-ai-gptel-openrouter-preferred-models)))
                  (and choice (seq-find (lambda (model)
                                          (string= (gptel--model-name model) choice))
                                        backend-models)))
                (car backend-models))))))

(defun pro-ai-gptel-refresh-openrouter-backend ()
  "Refresh OpenRouter free models and rebuild backend registration."
  (interactive)
  (when про-ии--gptel-available
    (let ((models (or (pro-ai-gptel-openrouter-free-models t)
                      pro-ai-gptel-openrouter-free-models-cache)))
      (when models
        (pro-ai-gptel--openrouter-set-backend models)
        (message "OpenRouter refreshed: %d free models" (length models))
        models))))

(defun pro-ai-gptel--разобрать-org-таблицу-цен (file)
  "Возвращает alist цен из org-таблицы FILE."
  (let ((alist nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (search-forward "| Модель" nil t)
        (forward-line 2)
        (while (re-search-forward "^| \([^|]+\) | \([^|]+\) | \([^|]+\) |" nil t)
          (let* ((model (intern (string-trim (match-string 1))))
                 (input (string-to-number (match-string 2)))
                 (output (string-to-number (match-string 3))))
            (push (cons model (list :input (/ input 1000.0)
                                    :output (/ output 1000.0)))
                  alist)))))
    (nreverse alist)))

(defun pro-ai-gptel-загрузить-тарифы ()
  "Загрузить тарифы из org-файла."
  (when (file-exists-p pro-ai-gptel-org-файл-цен)
    (setq pro-ai-тарифы-моделей
          (pro-ai-gptel--разобрать-org-таблицу-цен pro-ai-gptel-org-файл-цен))))

(add-hook 'emacs-startup-hook #'pro-ai-gptel-загрузить-тарифы)

(defcustom pro-ai-gptel-использовать-точный-подсчёт t
  "Использовать tiktoken для точного подсчёта токенов."
  :type 'boolean
  :group 'про-ии)

(defun pro-ai-gptel--подсчитать-токены-эвристика (text)
  "Эвристический подсчёт токенов (приблизительно)."
  (/ (length text) 4))

(defun pro-ai-gptel--подсчитать-токены-точно (text model)
  "Точный подсчёт через tiktoken."
  (if (executable-find "tiktoken-counter.py")
      (string-to-number
       (string-trim
        (shell-command-to-string
         (format "printf %s %s | tiktoken-counter.py --model %s"
                 (shell-quote-argument text)
                 ""
                 (shell-quote-argument model)))))
    (pro-ai-gptel--подсчитать-токены-эвристика text)))

(defun про-ai-gptel-подсчитать-токены (text &optional model)
  "Подсчитать количество токенов в TEXT."
  (if (and pro-ai-gptel-использовать-точный-подсчёт model)
      (pro-ai-gptel--подсчитать-токены-точно text model)
    (pro-ai-gptel--подсчитать-токены-эвристика text)))

(defun про-ai-gptel-посчитать-стоимость (вход выход model)
  "Посчитать стоимость в рублях."
  (let* ((тариф (or (assoc model pro-ai-тарифы-моделей)
                    (assoc 'default pro-ai-тарифы-моделей))))
    (when тариф
      (let ((цена-вход (plist-get (cdr тариф) :input))
            (цена-выход (plist-get (cdr тариф) :output)))
        (when (and цена-вход цена-выход)
          (+ (* вход цена-вход) (* выход цена-выход)))))))

(defcustom pro-ai-gptel-max-cost-rub 100.0
  "Максимальная стоимость одного запроса в рублях."
  :type 'number
  :group 'про-ии)

(defun про-ai-gptel--проверить-стоимость (вход выход model)
  "Хук для проверки стоимости перед запросом."
  (let ((стоимость (про-ai-gptel-посчитать-стоимость вход выход model)))
    (when (and стоимость (> стоимость pro-ai-gptel-max-cost-rub))
      (user-error "Слишком дорого: %s руб (лимит: %s)"
                  стоимость pro-ai-gptel-max-cost-rub))))

(defcustom pro-ai-gptel-proxyapi-key nil
  "Ключ ProxyAPI для OpenAI/Anthropic совместимых API."
  :type 'string
  :group 'про-ии)

(defcustom pro-ai-gptel-openai-key nil
  "Ключ OpenAI API."
  :type 'string
  :group 'про-ии)

(defcustom pro-ai-gptel-anthropic-key nil
  "Ключ Anthropic API."
  :type 'string
  :group 'про-ии)

(defcustom pro-ai-gptel-aitunnel-key nil
  "Ключ AITunnel API (https://aitunnel.ru)."
  :type 'string
  :group 'про-ии)

(defcustom pro-ai-gptel-deepseek-key nil
  "Ключ DeepSeek API."
  :type 'string
  :group 'про-ии)

(defun про-ии-set-key (key-name value)
  "Установить ключ KEY-NAME в значение VALUE."
  (pcase key-name
    ("proxyapi" (setq pro-ai-gptel-proxyapi-key value))
    ("openai" (setq pro-ai-gptel-openai-key value))
    ("anthropic" (setq pro-ai-gptel-anthropic-key value))
    ("deepseek" (setq pro-ai-gptel-deepseek-key value))
    ("aitunnel" (setq pro-ai-gptel-aitunnel-key value))
    (_ (user-error "Неизвестный ключ: %s" key-name))))

(defvar про-ии-gptel-system-role
  "Ты — продвинутый AI-ассистент, эксперт в программировании и не только."
  "Системная роль для GPTEL.")

(defvar про-ии--gptel-available (require 'gptel nil t)
  "Non-nil, если gptel доступен.")

(use-package gptel
  :ensure t
  :functions (gptel-make-openai gptel--get-api-key gptel-aibo-apply-last-suggestions)
  :bind (:map gptel-mode-map
              ("C-c RET" . gptel-send)
              ("C-c C-<return>" . gptel-send)
              ("C-c C-M-<return>" . pro/ai-send-without-context)
              ("C-c M-<return>" . pro/ai-send-without-context-aibo))
  :custom
  (gptel-default-mode 'org-mode)
  (gptel-org-branching-context nil)
  (gptel-log-level 'info)
  (gptel-context-restrict-to-project-files nil)
  (gptel--system-message "Ты — ИИ, живущий в Emacs под NIXOS. Отвечай в виде Org-mode. Любые списки представляй заголовками и пунктами Org.")
  :config
  (setq gptel-curl-file-size-threshold 0)

  (defun pro/ai-send-without-context ()
    "Отправить текущий prompt без контекста (stateless-режим)."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-send)))

  (defun pro/ai-send-without-context-aibo ()
    "Отправить prompt через gptel-aibo без контекста (если доступно)."
    (interactive)
    (let ((gptel-use-context nil))
      (when (fboundp 'gptel-aibo-send)
        (gptel-aibo-send)))))

(defun pro/ai--register-backend (name endpoint path auth-needed models &optional key)
  "Зарегистрировать GPTEL backend NAME на ENDPOINT."
  (when про-ии--gptel-available
    (gptel-make-openai name
      :host endpoint
      :endpoint path
      :key (or key (and auth-needed (gptel--get-api-key name)))
      :models models)))

(defun pro-ai-gptel--backend-model-names (backend)
  "Return BACKEND model names as strings."
  (mapcar #'gptel--model-name (gptel-backend-models backend)))

(defun pro-ai-gptel--select-preferred-model (models preferred)
  "Return the first preferred model name present in MODELS."
  (seq-find (lambda (name) (member name models)) preferred))

(defun pro-ai-gptel--preferred-model (models preferred)
  "Return the first preferred MODEL in MODELS."
  (seq-find (lambda (name) (member name models)) preferred))

(when про-ии--gptel-available
  (let ((openrouter-models (or (pro-ai-gptel-openrouter-free-models t)
                               pro-ai-gptel-openrouter-free-models-cache)))
    (when openrouter-models
      (pro-ai-gptel--openrouter-set-backend openrouter-models)))

  ;; AITunnel - best price/quality
  (when (and pro-ai-gptel-aitunnel-key
             (not (string-empty-p pro-ai-gptel-aitunnel-key)))
    (pro/ai--register-backend
     "AITunnel" "api.aitunnel.ru" "/v1/chat/completions" t
     pro-ai-gptel-aitunnel-preferred-models
     pro-ai-gptel-aitunnel-key)
    ;; Qwen full line via AITunnel
    (pro/ai--register-backend
     "Qwen" "api.aitunnel.ru" "/v1/chat/completions" t
     pro-ai-gptel-qwen-full-line
     pro-ai-gptel-aitunnel-key))

  (when (and pro-ai-gptel-proxyapi-key
             (not (string-empty-p pro-ai-gptel-proxyapi-key)))
    (pro/ai--register-backend
     "ProxyAPI-OpenAI" "api.proxyapi.xyz" "/v1/chat/completions" t
     '("gpt-4.1" "o4-mini" "gpt-4o" "gpt-4o-mini")
     pro-ai-gptel-proxyapi-key)
    (pro/ai--register-backend
     "ProxyAPI-Anthropic" "api.proxyapi.xyz" "/v1/chat/completions" t
     '("claude-sonnet-4-20250514" "claude-3-7-sonnet-20250227")
     pro-ai-gptel-proxyapi-key))

  (when (executable-find "ollama")
    (pro/ai--register-backend
     "Ollama" "localhost:11434" "/v1/chat/completions" nil
     '("qwen2.5-coder:latest" "llama3:latest" "mistral:latest")))

  (when pro-ai-gptel-deepseek-key
    (pro/ai--register-backend
     "DeepSeek" "api.deepseek.com" "/v1/chat/completions" t
     '("deepseek-chat" "deepseek-coder")
     pro-ai-gptel-deepseek-key))

  (pro/ai--register-backend
   "Perplexity" "api.perplexity.ai" "/chat/completions" t
   '("sonar-deep-research" "sonar-reasoning" "sonar"))

  (pro/ai--register-backend
   "Chutes" "llm.chutes.ai" "/v1/chat/completions" nil
   '("deepseek-ai/DeepSeek-V3-0324")))

(let ((openrouter-backend (gptel-get-backend pro-ai-gptel-openrouter-backend-name)))
  (when openrouter-backend
    (setq gptel-backend openrouter-backend)
    (let* ((models (gptel-backend-models openrouter-backend))
           (model-names (pro-ai-gptel--backend-model-names openrouter-backend))
           (choice (pro-ai-gptel--preferred-model
                    model-names
                    pro-ai-gptel-openrouter-preferred-models)))
      (setq gptel-model
            (or (and choice
                     (seq-find (lambda (model)
                                 (string= (gptel--model-name model) choice))
                               models))
                (car models))))))

(provide 'про-ии-core)
;;; про-ии-core.el ends here
