;;; про-ии.el --- Интеграция современных AI-сервисов в Emacs (GPTEL, ProxyAPI, Anthropic, DeepSeek и др.) -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.1
;; Keywords:  AI, GPT, Codeium, Whisper, ChatGPT
;; URL: https://example.com/про-ии
;; Package-Requires: ((emacs "29.1") (use-package "2.4") (gptel "0") (consult "1.7") (cape "0"))
;;
;;; Commentary:
;;
;; Этот файл — целостная, структурированная и сопровождаемая конфигурация
;; «литературного» вида для глубокой интеграции AI в Emacs на базе пакета GPTEL:
;;
;; Что умеет конфигурация:
;; - Создаёт пул backend’ов и моделей (OpenAI-совместимых и проксированных).
;; - Даёт удобные интерактивные команды для переключения backend/model.
;; - Отслеживает стоимость запросов (рубли), грузит прайс из org-таблицы.
;; - Точное или приближённое измерение токенов (через tiktoken-counter.py или эвристику).
;; - Централизованная настройка ключей, переменных окружения и заполнение их по умолчанию.
;; - Расширения: интеграция с gptel-commit, возможность отправки без контекста (stateless).
;;
;; Кратко по разделам:
;;  0. Введение, зависимости и базовые группы настроек
;;  1. Таблица цен за токены (рубли), загрузка из Org и поиск тарифа модели
;;  2. Калькуляция стоимости и подсчёт токенов (точный и эвристический)
;;  3. Ключи API и окружение (централизация)
;;  4. Настройка GPTEL и пул backend’ов (AI Tunnel, ProxyAPI: OpenAI/Anthropic, Chutes/DeepSeek)
;;  5. Интерактивные команды переключения backend/model (с Consult или fallback)
;;  6. Расширения: CAPE, хуки, пример интеграции (без включения по умолчанию)
;;  7. Генерация commit-сообщений с gptel-commit (стратегия — всегда gpt-4.1, stateless)
;;
;; Перед использованием:
;; - Настройте переменную =proxyapi-key= в приватном месте (она разойдётся по всем API).
;; - Рекомендуется установить tiktoken-counter.py (для точного подсчёта токенов любой модели).
;; - При необходимости уточните путь к org-файлу с ценами в переменной
;;   =pro-ai-gptel-pricing-org-file= (см. ниже). Если файла нет — будет использована пустая таблица.
;;
;; Загрузка:
;; - M-x load-file RET путь/до/про-ии.el RET
;;
;;; Code:

;;;; 0. Введение, зависимости и базовые группы настроек

;; Мы явно обозначаем группу Customize для всех опций этого модуля.
(defgroup про-ии nil
  "Глубокая интеграция AI-сервисов в Emacs на базе GPTEL."
  :group 'applications
  :prefix "pro-ai-gptel-")

(require 'установить-из)  ;; Универсальная функция установки пакетов (локальная/кастомная).
(require 'cape)           ;; CAPE — инфраструктура для подсказок и completion от AI и пр.

(eval-when-compile
  (require 'cl-lib))

;;;; 1. Таблица цен за токены (рубли), загрузка из Org и поиск тарифа модели

(defcustom pro-ai-gptel-org-файл-цен
  (expand-file-name "~/pro/тарифы-моделей.org")
  "Путь к org-файлу с таблицей цен за 1000 токенов (в рублях).
Ожидается таблица с колонками: | Модель | Вход | Выход |.
Будут считаны значения и преобразованы к стоимости за 1 токен."
  :type 'file
  :group 'про-ии)

(defcustom pro-ai-тарифы-моделей
  nil
  "Alist с ценами за токены в рублях: (MODEL . (:input RUB/1token :output RUB/1token)).
Внутреннее представление — стоимость за 1 токен, а не за 1000.
Данные обычно загружаются из =pro-ai-gptel-org-файл-цен=, но могут быть отредактированы вручную."
  :type '(alist :key-type symbol :value-type (plist :key-type symbol :value-type number))
  :group 'про-ии)

(defcustom pro-ai-gptel-курс-валюты 1.0
  "Курс пересчёта тарифов, если прайс не в рублях (обычно 1.0 = рубли).
Например, если у вас цены в $ за токен, здесь можно задать курс руб./$."
  :type 'number
  :group 'про-ии)

(defvar-local pro-ai-gptel-последняя-стоимость-руб nil
  "Последняя оцененная стоимость запроса в текущем буфере (float, рубли).
Обновляется после прихода ответа в gptel-mode (hook =gptel-post-response-functions=).")

(defun pro-ai-gptel--разобрать-org-таблицу-цен (file)
  "Возвращает alist цен из org-таблицы FILE.
Ожидается таблица вида:
| Модель | Вход | Выход |
Где Вход/Выход указаны в рублях за 1000 токенов. Функция приводит к руб./1токен."
  (let ((alist nil))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (search-forward "| Модель" nil t)
        ;; Пропустим заголовок и разделитель таблицы.
        (forward-line 2)
        (while (re-search-forward
                "^|\\s-*\\([^|]+\\)\\s-*|\\s-*\\([^|]+\\)\\s-*|\\s-*\\([^|]+\\)\\s-*|$"
                nil t)
          (let* ((model (string-trim (match-string 1)))
                 (input-1000 (string-to-number (string-trim (match-string 2))))
                 (output-1000 (string-to-number (string-trim (match-string 3)))))
            (unless (string-empty-p model)
              ;; Храним в руб./1токен, точность — float
              (push (cons (intern (replace-regexp-in-string "\\." "-" model))
                          (list :input (/ (float input-1000) 1000.0)
                                :output (/ (float output-1000) 1000.0)))
                    alist))))))
    (nreverse alist)))

(defun pro-ai-загрузить-тарифы-моделей ()
  "Загрузить или перезагрузить =pro-ai-тарифы-моделей= из =pro-ai-gptel-org-файл-цен=.
Если файл не найден, оставить текущее значение и вывести предупреждение."
  (interactive)
  (if (file-exists-p pro-ai-gptel-org-файл-цен)
      (condition-case err
          (progn
            (setq pro-ai-тарифы-моделей
                  (pro-ai-gptel--разобрать-org-таблицу-цен pro-ai-gptel-org-файл-цен))
            (message "AI pricing loaded: %d models from %s"
                     (length pro-ai-тарифы-моделей)
                     (abbreviate-file-name pro-ai-gptel-org-файл-цен)))
        (error
         (message "Ошибка чтения прайса: %s" (error-message-string err))))
    (message "Файл прайса не найден: %s (будет использоваться пустая таблица)"
             (abbreviate-file-name pro-ai-gptel-org-файл-цен))))

;; Попытка загрузить прайс на старте (не критично, в случае отсутствия файла всё продолжит работать).
(pro-ai-загрузить-тарифы-моделей)

(defun pro-ai-gptel-тарифы-модели (model)
  "Получить plist тарифов (:input :output) для модели MODEL (символ или строка).
Если точного совпадения нет, пробуем подобрать по префиксу имени модели.
Возвращает nil, если ничего не найдено."
  (let* ((name (if (symbolp model) (symbol-name model) (or model "")))
         (name (replace-regexp-in-string "\\." "-" name)))
    (or
     (cdr (assoc (intern name) pro-ai-тарифы-моделей))
     (let ((match (seq-find (lambda (x)
                              (and (consp x)
                                   (symbolp (car x))
                                   (string-prefix-p (symbol-name (car x)) name)))
                            pro-ai-тарифы-моделей)))
       (and match (cdr match))))))

;;;; 2. Калькуляция стоимости и подсчёт токенов (точный и эвристический)

(defun pro-ai-gptel-оценить-стоимость (n-in n-out model)
  "Оценить стоимость запроса по токенам prompt (N-IN) и response (N-OUT) для модели MODEL.
Возвращает сумму в рублях (float). Если тариф модели не найден — вернёт 0.0."
  (let* ((pricing (pro-ai-gptel-тарифы-модели model))
         (in (or (plist-get pricing :input) 0.0))
         (out (or (plist-get pricing :output) 0.0)))
    (/ pro-ai-gptel-курс-валюты
       (+ (/ (/ (float n-in) 1.0) in)
          (/ (/ (float n-out) 1.0) out)))))

(defun pro-ai-gptel--токены-из-диапазона (start end &optional model)
  "Подсчитать число токенов в диапазоне [START, END].
Пытается вызвать внешний =tiktoken-counter.py=, если он установлен:
- Скрипт читается из stdin, параметром передаётся MODEL (по умолчанию «gpt-4»).
- Если скрипт не найден/не исполняем — используется эвристика: длина текста / 3.5.
Возвращает целое число токенов (минимум 1)."
  (let* ((text (buffer-substring-no-properties start end))
         (tmpfile (make-temp-file "tik-tok-txt-"))
         (script (or (executable-find "tiktoken-counter.py")
                     (expand-file-name "~/.local/bin/tiktoken-counter.py")))
         (model (or model "gpt-4"))
         tokens)
    (with-temp-file tmpfile (insert text))
    (unwind-protect
        (if (and (file-exists-p script) (file-executable-p script))
            (setq tokens
                  (string-to-number
                   (with-temp-buffer
                     ;; stdin <- tmpfile, stdout -> current buffer
                     (call-process script tmpfile t nil model)
                     (buffer-string))))
          ;; Fallback-эвристика по символам
          (setq tokens (round (/ (max 1 (length text)) 3.5))))
      (ignore-errors (delete-file tmpfile)))
    (max 1 (truncate tokens))))

(defun pro-ai-gptel-после-ответа-посчитать-стоимость (beg end)
  "Hook-функция post-response для gptel: оценить стоимость запроса и вывести её в echo-area.
Диапазон ответа [BEG, END], prompt считается как (point-min .. BEG).
Обновляет =pro-ai-gptel-последняя-стоимость-руб= в текущем буфере."
  (when (and (bound-and-true-p gptel-mode) (> end beg))
    (let* ((response-len (pro-ai-gptel--токены-из-диапазона beg end))
           (prompt-tokens (pro-ai-gptel--токены-из-диапазона (point-min) beg))
           (model (if (boundp 'gptel-model) gptel-model 'unknown))
           (cost (pro-ai-gptel-оценить-стоимость prompt-tokens response-len model)))
      (setq-local pro-ai-gptel-последняя-стоимость-руб cost)
      (message "Стоимость запроса: ≈%.2f₽ (модель: %s, prompt: %d токенов, response: %d токенов)"
               cost model prompt-tokens response-len))))

;; Подключаем автоматический трекинг стоимости на каждое завершение ответа.
(add-hook (if (boundp 'gptel-post-response-hook)
              'gptel-post-response-hook
            'gptel-post-response-functions)
          #'pro-ai-gptel-после-ответа-посчитать-стоимость)

;;;; 3. Ключи API и окружение (централизация)

;; Объявление переменных, которые могут быть заданы в приватных файлах/переменных окружения:
(defvar pro-ai-aitunnel-хост nil
  "Базовый хост для кастомного AI-туннеля (без протокола).")
(defvar pro-ai-aitunnel-key nil
  "API-key для AI-туннеля.")
(defvar pro-ai-aitunnel-backend nil
  "Объект backend для AI-туннеля (создаётся при конфигурировании GPTEL).")
(defvar pro-ai-chutes-key nil
  "API-key для сервиса Chutes (DeepSeek).")
(defvar pro-ai-proxyapi-хост "api.proxyapi.ru"
  "Базовый хост ProxyAPI.")
(defvar pro-ai-proxyapi-key nil
  "Единый ключ для ProxyAPI.")
(defvar pro-ai-openrouter-хост "openrouter.ai"
  "Хост OpenRoouter.")
(defvar pro-ai-openrouter-key nil
  "Единый ключ для OpenRouter.")
(defvar pro-ai-perplexity nil
  "Единый ключ для Perplexity.")

;;;; 4. GPTEL: настройка и пул backend’ов (AI Tunnel, ProxyAPI: OpenAI/Anthropic, Chutes/DeepSeek)

(use-package gptel
  :ensure t
  :functions (gptel-make-openai gptel--get-api-key gptel-aibo-apply-last-suggestions)
  :bind (:map gptel-mode-map
              ("C-c RET"           . gptel-send)
              ("C-c C-<return>"    . gptel-send)
              ("M-RET"             . pro-ai-отправить-без-контекста)
              ("C-c M-RET"         . pro-ai-отправить-без-контекста-aibo))
  ;;:hook ((gptel-mode . tab-line-mode))
  :custom
  (gptel-default-mode 'org-mode)                ;; Ответы в org-mode
  (gptel-org-branching-context nil)             ;; Без разветвления контекста по умолчанию
  (gptel-log-level 'info)
  ;; Примечание: Используется внутренний var =gptel--system-message= — в API GPTEL он может меняться.
  ;; Если увидите варнинги: замените на актуальную переменную системного сообщения из GPTEL.
  (gptel--system-message
   "Ты — ИИ, живущий в Emacs под NIXOS. Отвечай в виде Org-mode. Любые списки представляй заголовками и пунктами Org.")
  :config
  ;; Быстрая отправка без контекста (stateless)
  (defun pro-ai-отправить-без-контекста ()
    "Отправить текущий prompt без контекста (stateless-режим)."
    (interactive)
    (let ((gptel-use-context nil))
      (gptel-send)))

  ;; Быстрая отправка через gptel-aibo без контекста (если установлен расширитель gptel-aibo)
  (defun pro-ai-отправить-без-контекста-aibo ()
    "Отправить prompt через gptel-aibo без контекста (если доступно)."
    (interactive)
    (let ((gptel-use-context nil))
      (when (fboundp 'gptel-aibo-send)
        (gptel-aibo-send))))

  ;; --- AI Tunnel (кастомный прокси/сервер OpenAI API) ---
  (setq pro-ai-aitunnel-backend
        (gptel-make-openai "AI Tunnel"
          :protocol "https"
          :host pro-ai-aitunnel-хост
          :endpoint "/v1/chat/completions"
          :stream t
          :key pro-ai-aitunnel-key
          :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
          :models (append
                   '("gpt-5" "gpt-4.5" "gpt-4.1" "gpt-4.1-mini" "gpt-4.1-nano"
                     "o3" "o3-mini" "o1-pro" "o1" "o1-mini" "o4-mini"
                     "gpt-4o-search-preview" "gpt-4o-mini-search-preview"
                     "gpt-4o-audio-preview" "gemini-2.5-pro-preview" "gemini-2.5-flash" "gemini-2.5-flash-lite"
                     "claude-sonnet-4" "claude-opus-4" "claude-opus-4.1" "llama-4-maverick"
                     "deepseek-r1" "deepseek-r1-fast" "deepseek-chat" "grok-3-mini-beta" "grok-4")
                   gptel--openai-models)))

  ;; --- ProxyAPI: OpenAI-совместимый endpoint ---
  (gptel-make-openai "Proxy OpenAI"
    :protocol "https"
    :host pro-ai-proxyapi-хост
    :endpoint "/openai/v1/chat/completions"
    :stream t
    :key pro-ai-proxyapi-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models (append
             '("gpt-4o-search-preview" "gpt-4o-mini-search-preview" "gpt-5" "gpt-4.1" "o4-mini"
               "o4-mini-high" "gpt-4.5-preview" "o1-mini" "o1-pro"
               "dall-e-3" "gpt-4o" "gpt-4o-mini" "gpt-4o-audio-preview")
             gptel--openai-models))

  (gptel-make-openai "Openrouter"
    :protocol "https"
    :host pro-ai-openrouter-хост
    :endpoint "/api/v1/chat/completions"
    :stream nil
    :key pro-ai-openrouter-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("agentica-org/deepcoder-14b-preview:free"))

  (gptel-make-openai "Perplexity"
    :protocol "https"
    :host "api.perplexity.ai"
    :endpoint "/chat/completions"
    :stream t
    :key pro-ai-perplexity-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("sonar-deep-research" "sonar-reasoning" "sonar"))


  ;; --- ProxyAPI: Anthropic (Claude) через OpenAI-совместимую обёртку (экспериментально) ---
  ;; Внимание: это путь через совместимый интерфейс. Если API несовместим — возможно потребуются
  ;; нативные адаптеры/эндпоинты GPTEL для Anthropic.
  (gptel-make-openai "ProxyAPI Anthropic"
    :protocol "https"
    :host pro-ai-proxyapi-хост
    :endpoint "/anthropic/v1/messages"
    :stream nil
    :key gptel-api-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("claude-3-5-sonnet-20241022" "claude-3-opus-20240229"))

  ;; --- Chutes: DeepSeek ---
  (gptel-make-openai "Chutes"
    :protocol "https"
    :host "llm.chutes.ai"
    :endpoint "/v1/chat/completions"
    :stream nil
    :key pro-ai-chutes-key
    :header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
    :models '("deepseek-ai/DeepSeek-V3-0324"))

  ;; Бэкенд/модель по умолчанию — можно интерактивно менять
  (setq gptel-backend (gptel-get-backend "AI Tunnel"))
  (setq gptel-model 'gpt-5))

;;;; 5. Интерактивные команды: переключение backend и модели (с Consult или fallback)

(defun pro-ai--read-with-consult-or-completing-read (prompt candidates &optional require-match initial annotate)
  "Вспомогательная функция: выбрать строку из CANDIDATES.
Если доступен =consult=, используем =consult--read=, иначе — =completing-read=.
PROMPT — строка приглашения. REQUIRE-MATCH, INITIAL, ANNOTATE — дополнительные параметры."
  (cond
   ((require 'consult nil t)
    (consult--read
     candidates
     :prompt prompt
     :require-match require-match
     :initial initial
     :annotate annotate))
   (t
    (completing-read prompt candidates nil require-match nil nil initial))))

;; (defun pro-ai-org-auto-enable-gptel-mode-by-heading ()
;;   "Автоматически включает gptel-mode, если в Org-файле есть заголовки, содержащие 'gptel'."
;;   (when (and (derived-mode-p 'org-mode)
;;              (save-excursion
;;                (goto-char (point-min))
;;                (re-search-forward "^\\*+.*gptel" nil t)))
;;     (unless (bound-and-true-p gptel-mode)
;;       (gptel-mode 1)
;;       (message "gptel-mode включён по gptel-заголовку"))))

;; (add-hook 'org-mode-hook #'pro-ai-org-auto-enable-gptel-mode-by-heading)

(defun pro/ai-switch-backend ()
  "Интерактивный выбор и активация =gptel-backend= среди известных бэкендов."
  (interactive)
  (require 'gptel)
  (let* ((known-backends (and (boundp 'gptel--known-backends) gptel--known-backends))
         (backends (mapcar #'car known-backends))
         (current (and (boundp 'gptel-backend) gptel-backend))
         (cur-str (or (and current (gptel-backend-name current)) "")))
    (if (null backends)
        (user-error "Нет зарегистрированных бэкендов GPTEL")
      (let* ((choice (pro-ai--read-with-consult-or-completing-read
                      (format "Выберите AI backend (текущий: %s): " cur-str)
                      backends t)))
        (when (and choice (not (string-empty-p choice)))
          (setq gptel-backend (gptel-get-backend choice))
          (message "Переключено на backend: %s" choice)
          (force-mode-line-update t))))))

(defun pro/ai-switch-model ()
  "Интерактивный выбор модели из всех доступных во всех бэкендах."
  (interactive)
  (require 'gptel)
  ;; Собираем все модели из всех бэкендов. Для удобства отображаем «Бэкенд:Модель».
  (let* ((known (and (boundp 'gptel--known-backends) gptel--known-backends)))
    (when (null known)
      (user-error "Нет зарегистрированных бэкендов GPTEL"))
    (let* ((models-alist
            (cl-loop
             for (name . backend) in known
             nconc
             (cl-loop
              for model in (gptel-backend-models backend)
              collect (let* ((model-name (gptel--model-name model))
                             (full-name (concat (string-trim-right name) ":" model-name))
                             (desc (or (get model :description) ""))
                             (item (list :name name :model model :desc desc)))
                        (cons full-name item)))))
           (current-backend-name (and (boundp 'gptel-backend) (gptel-backend-name gptel-backend)))
           (current-model-name (and (boundp 'gptel-model) (gptel--model-name gptel-model)))
           (initial (and current-backend-name current-model-name
                         (concat current-backend-name ":" current-model-name)))
           (candidates (mapcar #'car models-alist))
           (annotate (lambda (cand)
                       (let* ((item (cdr (assoc cand models-alist)))
                              (desc (plist-get item :desc)))
                         (when (and desc (not (string-empty-p desc)))
                           (concat " — " desc)))))
           (choice (pro-ai--read-with-consult-or-completing-read
                    "Выберите модель (формат: Бэкенд:Модель): "
                    candidates t initial annotate)))
      (when choice
        (let* ((item (cdr (assoc choice models-alist)))
               (backend-name (plist-get item :name))
               (model (plist-get item :model))
               (backend (gptel-get-backend backend-name)))
          (setq gptel-backend backend
                gptel-model model)
          (message "Переключено на модель: %s (бэкенд: %s)" (gptel--model-name model) backend-name)
          (force-mode-line-update t))))))

;;;; 6. Расширения: CAPE, хуки, интеграции (не включены по умолчанию)

;; Если используете Corfu/CAPE — можно подключить подсказки от gptel как один из источников:
;; (add-hook 'text-mode-hook
;;           (lambda ()
;;             (add-to-list (make-local-variable 'completion-at-point-functions)
;;                          #'cape-gptel)))

;;;;; СТруктурировать...............:

(use-package gptel-aibo
  :ensure t
  ;;:hook ((gptel-aibo-mode . tab-line-mode))
  :bind (:map gptel-aibo-mode-map
              ("C-c RET"      . gptel-aibo-send)
              ("C-c C-<return>"      . gptel-aibo-send)
              ("C-c M-RET"    . pro-ai-отправить-без-контекста-aibo)))

;;;; Настройка gptel-quick для быстрых запросов

(use-package gptel-quick
  :after gptel
  :init
  (установить-из :repo "karthink/gptel-quick")
  (setq gptel-quick-backend gptel-backend)
  (setq gptel-quick-model 'gpt-4.1))

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
  (plist-put minuet-openai-compatible-options :end-point (concat "https://" pro-ai-proxyapi-хост "/openai/v1/chat/completions"))
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

(defun pro-ai-gptel-git-command (command)
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
;;  :function #'pro-ai-gptel-git-command
;;  :name "run_git_command"
;;  :description "Run any git command in the root of the current project. The argument is a valid git command, e.g. 'status', 'log --oneline', or 'diff HEAD~1'. Returns the raw output of git (stdout and stderr)."
;;  :args (list
;;         (list :name "command"
;;               :type 'string
;;               :description "A valid git command to run, e.g. 'status', 'log --oneline', or 'diff HEAD~1'.")))

;; (add-to-list 'gptel-tools (gptel-get-tool "run_git_command")))

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
  :bind (("C-c <tab>" . pro-ai-дополнение-codeium))
  :config
  (defun pro-ai-codeium-on ()
    "Включить Codeium."
    (interactive)
    (codeium-init)
    (add-to-list 'completion-at-point-functions #'codeium-completion-at-point))
  (defun pro-ai-codeium-off ()
    "Отключить Codeium."
    (interactive)
    (setq completion-at-point-functions
          (remove #'codeium-completion-at-point completion-at-point-functions)))
  (defun pro-ai-codeium-toggle ()
    "Переключить состояние Codeium."
    (interactive)
    (if (memq #'codeium-completion-at-point completion-at-point-functions)
        (codeium-off)
      (codeium-on)))
  (defalias 'pro-ai-дополнение-codeium (cape-capf-interactive #'codeium-completion-at-point))
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
(defun pro-ai-gptel-set-model ()
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

(use-package context-navigator
  :load-path "~/Code/context-navigator/lisp"
  ;; :straight (context-navigator
  ;;            :type git
  ;;            :host github
  ;;            :repo "11111000000/context-navigator")
  :custom

  ;; ;; Basics
  (context-navigator-language 'ru)
  (context-navigator-global-key "C-c n")           ;; recommended global key for the transient
  ;; (context-navigator-autoload t)                   ;; load pieces on first use
  ;; (context-navigator-autosave t)                   ;; auto-save contexts
  ;; (context-navigator-default-auto-project-switch t)
  ;; (context-navigator-default-push-to-gptel t)
  ;; (context-navigator-sidebar-width 36)             ;; sidebar width in columns

  ;; ;; Sidebar look & feel
  ;; (context-navigator-controls-style 'icons)        ;; compact header/footer controls
  ;; (context-navigator-highlight-active-group t)     ;; highlight current group
  ;; (context-navigator-auto-open-groups-on-error t)  ;; open groups list if a load error occurs
  ;; (context-navigator-gptel-indicator-poll-interval 0.8) ;; keep gptel indicators fresh

  ;; ;; Render options
  ;; (context-navigator-render-indicator-style 'icons) ;; tiny lamps via all-the-icons
  ;; (context-navigator-render-show-path nil)          ;; show right-aligned paths
  ;; (context-navigator-render-truncate-name 64)       ;; truncate long item names

  ;; ;; Icons provider
  ;; (context-navigator-enable-icons t)               ;; turn on icons in the sidebar
  ;; (context-navigator-icons-disable-on-remote t)    ;; be gentle on TRAMP connections

  ;; ;; Footer “open buffers” counter
  ;; (context-navigator-openable-count-ttl 0.3)       ;; cache TTL for counting openable buffers
  ;; (context-navigator-openable-soft-cap 100)        ;; soft cap when opening many buffers
  ;; (context-navigator-openable-remote-mode 'lazy)   ;; 'off | 'lazy | 'strict for remote projects

  ;; ;; Project switching and persistence
  ;; (context-navigator-context-switch-interval 0.7)  ;; throttle auto-project switching
  ;; (context-navigator-create-default-group-file t)  ;; create a default group file automatically

  ;; ;; Universal add (Dired recursion) size limit
  ;; (context-navigator-max-file-size (* 2 1024 1024)) ;; skip files larger than 2MB

  ;; ;; Internationalization
  ;; (context-navigator-language 'auto)               ;; en/ru/fr/de/es or auto-from-locale

  ;; ;; Protect your layout when the sidebar is open
  ;; (context-navigator-protect-sidebar-windows t)

  :config
  ;; Enable the global minor mode after customizing.
  (context-navigator-mode 1)

  ;; Tip: If you enabled icons, install fonts once:
  ;; M-x all-the-icons-install-fonts
  )

;; (use-package gptel-secrets
;;   :after gptel
;;   :load-path "~/Code/gptel-secrets"
;;   :init
;;   (setq gptel-secrets-locale 'ru
;;         gptel-secrets-action 'confirm
;;         gptel-secrets-enable t)
;;   :config
;;   (gptel-secrets-mode 1))

;; --- Автоматически включать gptel-mode при открытии Org-файла с PROPERTIES, связанными с gptel ---

;; (defun pro-ai-org-auto-enable-gptel-mode ()
;;   "Автоматически включает gptel-mode, если Org-файл содержит PROPERTIES, связанные с gptel."
;;   (when (and (derived-mode-p 'org-mode)
;;              (save-excursion
;;                (goto-char (point-min))
;;                (re-search-forward
;;                 "^\\s-*\\(:PROPERTIES:\\)?\\s-*$\\([^:]*\\(:gptel\\|gptel-\\)[^:]*\\|\\(:gptel\\)[^\\n]*\\)*"
;;                 nil t)))
;;     (unless (bound-and-true-p gptel-mode)
;;       (gptel-mode 1)
;;       (message "gptel-mode включён автоматически (найдены PROPERTIES, связанные с gptel)"))))

;; (add-hook 'org-mode-hook #'pro-ai-org-auto-enable-gptel-mode)

(require 'gptel-aibo-planner)
;;;###autoload
(defun pro-ai-gptel-aibo-apply-actions-from-region (beg end)
  "Parse and execute <OP> actions from region between BEG and END.
Понимает формат операций gptel-aibo:
  - <OP> MODIFY `BUFNAME` ... с парами *SEARCH*/*REPLACE*
  - <OP> CREATE <FILEPATH> ... с содержимым в блоке
  - <OP> DELETE <FILEPATH>

  В Org-буферах используется org-парсер."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Нет выделенного региона"))
  (let* ((text (buffer-substring-no-properties beg end))
         (parser (if (derived-mode-p 'org-mode)
                     (gptel-aibo-make-action-org-parser)
                   (gptel-aibo-make-action-parser)))
         (ops (gptel-aibo-parse-action parser text)))
    (cond
     ;; Успешный парсинг — ops это список операций
     ((and (listp ops) (not (eq (car-safe ops) 'error)))
      (let* ((target (or (and (boundp 'gptel-aibo--working-buffer)
                              (buffer-live-p gptel-aibo--working-buffer)
                              gptel-aibo--working-buffer)
                         (current-buffer)))
             (ok 0) (fail 0))
        (with-current-buffer target
          (dolist (op ops)
            (condition-case err
                (progn
                  (gptel-aibo-execute op)
                  (setq ok (1+ ok)))
              (error
               (setq fail (1+ fail))
               (message "gptel-aibo: ошибка при выполнении %S: %S" op err)))))
        (message "gptel-aibo: применено %d операций, ошибок %d" ok fail)))
     ;; Ошибка парсера — обычно возвращается (error . DETAIL) или подобная структура
     (t
      (message "gptel-aibo: ошибка парсинга: %S" ops)))))

(provide 'про-ии)

;;; про-ии.el ends here
