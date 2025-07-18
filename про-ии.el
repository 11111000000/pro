;;; про-ии.el --- Интеграция искусственного интеллекта в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: AI, GPT, Codeium, Whisper, ChatGPT
;; URL: https://example.com/про-ии
;;
;;; Commentary:
;; Конфиг для глубокой интеграции современных AI-сервисов (GPT, Anthropic, DeepSeek, Gemini и др.) в Emacs.
;; Позволяет работать с нейросетями, трекать стоимость в рублях, быстро переключать backend/model,
;; удобно использовать org, получать аудио/картинки и многое другое прямо в удобном редакторе.
;;
;; Кратко по секциям:
;;  0. Зависимости и пакеты
;;  1. Кастомная и расширяемая таблица цен за токены (цены для разных моделей; все в рублях)
;;  2. Калькуляция стоимости запроса и трекинг расходов
;;  3. API-ключи и настройка переменных окружения
;;  4. Настройка пакета gptel и backend’ов (OpenAI, Proxyapi, Anthropic, DeepSeek и т.д.)
;;  5. Быстрое переключение backend/model через интерактивные команды
;;  6. Другое: хуки, локальные функции, расширения
;;
;; Перед использованием:
;;  - Задайте переменную `proxyapi-key` в вашем приватном файле или .emacs (ее подхватят все API).
;;  - Рекомендуется установить tiktoken-counter.py (для точного подсчета токенов любой модели).
;;  
;; Загружайте файл: M-x load-file RET путь/до/про-ии.el RET

;;; Code:
;;;; 0. Импорт зависимостей, utility-скриптов и прогреваем окружение
(require 'установить-из)  ;; Универсальная функция установки пакетов из репозиториев.
(require 'cape)           ;; CAPE — дополнение автодополнения и AI-подсказок.

;;;; 1. Таблица цен по токенам (чтобы считать траты, удобно сравнивать и планировать)
(defcustom pro-gptel-model-pricing-alist
  '(
    ;; === Anthropic ===
    (claude-sonnet-4-20250514         :input 0.7344   :output 3.672)
    (claude-opus-4-20250514           :input 3.672    :output 18.36)
    (claude-3-sonnet-20240229         :input 0.7344   :output 3.672)
    (claude-3-opus-20240229           :input 3.672    :output 18.36)
    (claude-3-haiku-20240307          :input 0.0612   :output 0.306)
    (claude-3-7-sonnet-20250219       :input 0.7344   :output 3.672)
    (claude-3-5-sonnet-20241022       :input 0.7344   :output 3.672)
    (claude-3-5-sonnet-20240620       :input 0.7344   :output 3.672)
    (claude-3-5-haiku-20241022        :input 0.2448   :output 1.224)
    ;; === DeepSeek ===
    (deepseek-reasoner                :input 0.13464  :output 0.53611)
    (deepseek-chat                    :input 0.06610  :output 0.26928)
    ;; === Google Gemini ===
    (gemini-2.5-pro-preview-06-05     :input 0.306    :output 2.448)
    ... ; смело добавляйте новые модели в этот список!
    ;; === OpenAI ===
    (o4-mini-2025-04-16               :input 0.26928  :output 1.07712)
    ...
    ;; === (Пример) генерация изображений или TTS/Audio — задавайте :input без :output
    ;; (gpt-image-1-medium-1024x1024 :input 10.2)
    ;; (tts-1-hd :input 7.344)
    ;; (whisper-1 :input 1.47)
   )
  "Alist с ценами за токены: (MODEL . (:input RUB/1k-токенов :output RUB/1k-токенов))
Цены по 1000 токенов, для рублевой оценки (можно скорректировать через customize)."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type number))
  :group 'gptel)

(defcustom pro-gptel-currency-rate 1.0
  "Курс пересчёта тарифов, если не в рублях (обычно 1.0 = рубли).
Если у вас тарифы в $, здесь можно задать актуальный курс."
  :type 'number
  :group 'gptel)

(defvar-local pro-gptel-last-cost-rub nil
  "Последняя рассчитанная стоимость токенов текущего запроса (float, рубли).
Показывается в header-line, можно использовать вне gptel.")

;;;; 2. Функции оценки стоимости запросов и точного подсчёта токенов
(defun pro-gptel-model-pricing (model)
  "Получить plist (:input :output ...) для модели MODEL (символ или строка).
Если нет точного совпадения, попробует подобрать по префиксу."
  (let* ((name (if (symbolp model) (symbol-name model) model)))
    (or
     (cdr (assoc (intern name) pro-gptel-model-pricing-alist))
     (let ((match (seq-find (lambda (x)
                              (and (consp x)
                                   (symbolp (car x))
                                   (string-prefix-p name (symbol-name (car x)))))
                            pro-gptel-model-pricing-alist)))
       (and match (cdr match)))
     )))

(defun pro-gptel-estimate-cost (n-in n-out model)
  "Оценить стоимость запроса по количеству токенов prompt (N-IN) и response (N-OUT) для модели MODEL.
Возвращает сумму в рублях."
  (let* ((pricing (pro-gptel-model-pricing model))
         (in (or (plist-get pricing :input) 0))
         (out (or (plist-get pricing :output) 0)))
    (* pro-gptel-currency-rate
       (+ (* (/ (float n-in) 1000) in)
          (* (/ (float n-out) 1000) out)))))

(defun pro-gptel--tokens-from-info (start end &optional model)
  "Точным образом подсчитать количество токенов в диапазоне [START,END] (или всего буфера).
Пытается вызвать внешний tiktoken-counter.py; если не найден — делит количество символов на 3.5 (эвристика)."
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
      ;; fallback: эвристика по символам
      (setq tokens (round (/ (length text) 3.5))))
    (delete-file tmpfile)
    (max 1 tokens)))

(defun pro-gptel-post-response-cost (beg end)
  "Обработчик post-response: выводит стоимость последнего запроса для gptel-mode (от start до end).
Срабатывает в gptel-mode у буфера после прихода ответа."
  (when (and (bound-and-true-p gptel-mode) (> end beg))
    (let* ((buf (current-buffer))
           (response-len (pro-gptel--tokens-from-info beg end))
           (prompt-tokens (pro-gptel--tokens-from-info (point-min) beg))
           (model (if (boundp 'gptel-model) gptel-model 'unknown))
           (cost (pro-gptel-estimate-cost prompt-tokens response-len model)))
      (setq-local pro-gptel-last-cost-rub cost)
      (message "Стоимость запроса: ≈%.2f₽ (модель: %s, prompt: %d токенов, response: %d токенов)"
               cost model prompt-tokens response-len))))

;; Включаем автоматический трекинг расходов при каждом ответе в gptel-mode!
(add-hook 'gptel-post-response-functions #'pro-gptel-post-response-cost)

;;;; 3. Настроить переменные API/ключей и окружение

;; Объявление некоторых переменных бэкендов; могут быть определены в другом приватном файле:
(defvar tunnelai-url)
(defvar tunnelai-key)
(defvar tunnelai-backend)
(defvar chutes-api-key)
(defvar proxyapi-url)
(defvar proxyapi-key)

;; Автоматизация настройки ключей: если proxyapi-key определена, ставим ее всюду
(when (boundp 'proxyapi-key)
  (setq-default openai-key proxyapi-key)
  (setq-default gptel-api-key proxyapi-key)
  (setq-default chatgpt-shell-openai-key proxyapi-key)
  (setq-default dall-e-shell-openai-key proxyapi-key)
  (setenv "OPENAI_API_KEY" proxyapi-key))

;;;; 4. Настройка пакета GPTEL и множественных AI бекендов

(use-package gptel
  :ensure t
  :functions (gptel-make-openai gptel--get-api-key gptel-aibo-apply-last-suggestions)
  :bind (:map gptel-mode-map
              ("C-c RET"      . gptel-send)
              ("C-c C-<return>"      . gptel-send)
              ("M-RET"        . pro/gptel-send-no-context)
              ("C-c M-RET"    . pro/gptel-aibo-no-context))
  :custom
  ((gptel-default-mode 'org-mode)                ;; Использовать org-mode для всего AI
   (gptel-org-branching-context nil)             ;; Без автосоздания новых веток в org
   (gptel-api-key proxyapi-key)                  ;; API-key централизованный
   (gptel-log-level 'info)
   (gptel--system-message 
    "Ты — ИИ, живущий в Emacs под NIXOS. Отвечай в виде Org-mode."))
  :config
  ;; Реализация быстрой отправки без контекста (M-RET)
  (defun pro/gptel-send-no-context ()
    "Отправить prompt без контекста (stateless режим)."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-send)))
  (defun pro/gptel-aibo-no-context ()
    "Отправить prompt через gptel-aibo без контекста."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-aibo-send)))
  ;; Включить расширенное хранилище контекста, если используется (локальная библиотека)
  (require 'gptel-context-store nil t)

  ;; Создаем разнообразие бэкендов (вы делаете свой AI-пул по вкусу)
  ;; --- Прокси Туннель (например, самостоятельный API proxy на своем сервере) ---
  (setq tunnelai-backend
        (gptel-make-openai "AI Tunnel"
         :protocol "https"
         :host tunnelai-url
         :endpoint "/v1/chat/completions"
         :stream t
         :key tunnelai-key
         :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
         :models (append
                  '("gpt-4.5-preview" "gpt-4.1" "gpt-4.1-mini" "gpt-4.1-nano"
                    "o3" "o3-mini" "o1-pro" "o1" "o1-mini" "o4-mini"
                    "gpt-4o-search-preview" "gpt-4o-mini-search-preview"
                    "gpt-4o-audio-preview" "gemini-2.5-pro-preview-03-25"
                    "gemini-2.5-flash-preview-05-20"
                    "deepseek-r1" "deepseek-chat" "grok-3-mini-beta" "grok-4")
                  gptel--openai-models)))

  ;; --- ProxyAPI: центральный публичный российский прокси разных AI ---
  (gptel-make-openai "Proxy OpenAI"
    :protocol "https"
    :host "api.proxyapi.ru"
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key gptel-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models (append
             '("gpt-4o-search-preview" "gpt-4o-mini-search-preview" "gpt-4.1" "o4-mini"
               "o4-mini-high" "gpt-4.5-preview" "o1-mini" "o1-pro"
               "dall-e-3" "gpt-4o" "gpt-4o-mini" "gpt-4o-audio-preview")
             gptel--openai-models))

  ;; --- ProxyAPI Anthropic (Claude) ---
  (gptel-make-openai "ProxyAPI Anthropic"
    :protocol "https"
    :host "api.proxyapi.ru"
    :endpoint "/anthropic/v1/messages"
    :stream nil
    :key gptel-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("claude-3-5-sonnet-20241022" "claude-3-opus-20240229"))

  ;; --- Chutes (DeepSeek, отдельный бэкенд) ---
  (gptel-make-openai "Chutes"
    :protocol "https"
    :host "llm.chutes.ai"
    :endpoint "/v1/chat/completions"
    :stream nil
    :key chutes-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("deepseek-ai/DeepSeek-V3-0324"))

  ;; Бэкенд и модель по умолчанию (можно менять в интерактивном режиме)
  (setq gptel-backend (gptel-get-backend "AI Tunnel"))
  (setq gptel-model 'gpt-4.1))

;;;; 5. Быстрое меню интерактивного переключения backend через consult/completing-read

(defun pro/gptel-switch-backend ()
  "Интерактивный выбор и активация `gptel-backend` среди всех известных бэкендов (через consult/completing-read)."
  (interactive)
  (require 'gptel)
  (require 'consult)
  (let* ((known-backends (and (boundp 'gptel--known-backends) gptel--known-backends))
         (backends (mapcar #'car known-backends))
         (current (and (boundp 'gptel-backend) gptel-backend))
         (str (or (and current (gptel-backend-name current)) "")))
    (let* ((choice (consult--read
                    backends
                    :prompt (format "Выберите AI backend (текущий: %s): " str)
                    :require-match t
                    :category 'backend)))
      (when choice
        (setq gptel-backend (gptel-get-backend choice))
        (message "Переключено на backend: %s" choice)
        (force-mode-line-update t)))))

;;;; 6. Друзья-расширения: хуки, автозагрузка, расширения других пакетов (Cape и др.)

;; Автодополнение от AI (Cape — в пару к corfu/completion-at-point)
;; Можно добавить в свой `completion-at-point-functions`
;; (add-hook 'text-mode-hook (lambda () (add-to-list (make-local-variable 'completion-at-point-functions) #'cape-gptel)))

(provide 'про-ии)

;;; про-ии.el ends here
