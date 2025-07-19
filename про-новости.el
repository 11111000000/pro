;;; про-новости.el --- Интеграция RSS, AI и аналитики новостей для Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр Косов <11111000000@email.com>
;; Версия: 4.3
;; Keywords: news, RSS, Elfeed, AI, аналитика, дайджест, Emacs
;; URL: https://github.com/peterkosov/pro-news-ai
;;
;;; Commentary:
;;
;; Данный модуль превращает Emacs в персональную редакцию новостей с глубокой аналитикой,
;; объединяя считывание RSS-лент (через elfeed) с современными AI-сервисами (gptel, аналогами из про-ии.el).
;;
;; Основные возможности:
;;  • Асинхронный сбор новостей из базы elfeed с обработкой чанками, чтобы не блокировать интерфейс.
;;  • Фильтрация и пакетирование новостных заголовков по регулярным выражениям и стоп-словам.
;;  • Формирование красивого, человекочитаемого запроса из набора новостей для отправки на AI-анализ.
;;  • Макрос для создания интерактивных сценариев, позволяющий быстро генерировать команды с кастомными параметрами.
;;  • Интеграция с gptel: создание нового чат-буфера, отправка запроса, обработка и логирование ответа.
;;  • Автоматическое логирование каждого запроса в отдельные org-файлы, а также возможности по сравнению,
;;    отображению истории и анализу новых тем из логов.
;;  • Дополнительное меню для выбора и управления инструментами анализа, просмотра истории, семантической картой тем.
;;
;; Структура кода:
;;  0. Зависимости, группы настроек и базовые переменные.
;;  1. Асинхронное получение новостей через обработку чанков.
;;  2. Функции фильтрации, пакетирования и форматирования новостных данных.
;;  3. Макрос для создания интерактивных сценариев (команд) с параметрами сбора и отправки.
;;  4. Логирование аналитических запросов: формирование и запись org-лога.
;;  5. Интеграция с gptel: отправка запроса в AI, создание контекста и вызов gptel-send.
;;  6. Обработка ответа AI (hook): логирование ответа и запуск ассистента анализа новых тем.
;;  7. Дополнительные утилиты: просмотр истории, сравнение логов, семантическая карта, копирование дайджеста и предпросмотр.
;;  8. Главное меню инструментов и динамическое обновление пользовательских сценариев.
;;
;; Улучшения в версии 4.3:
;;  - Расширенная документация для каждой секции.
;;  - Более детальное логирование и отладочная информация через переменную отладки.
;;  - Повышена устойчивость при отсутствии базы elfeed или при ошибках записи в лог.
;;  - Добавлена интеграция с функциями AI из модуля про-ии.el (общий стиль работы с gptel).
;;
;;; Code:

;;;; 0. Зависимости, группы настроек и базовые переменные

(require 'subr-x)
(require 'use-package)
(require 'cl-lib)
(require 'elfeed)
(require 'gptel)

(defgroup про-новости nil
  "Настройки для новостного дайджеста с AI-аналитикой в Emacs."
  :group 'applications)

(defcustom про-новости-отладка t
  "Если non-nil, выводить отладочные сообщения для операций этого модуля.
Полезно для диагностики работы и проверки корректности асинхронной обработки."
  :type 'boolean
  :group 'про-новости)

(defcustom про-новости-промт-по-умолчанию
  "Подытожь день: убери повторы, отфильтруй рекламу и дай аналитическую оценку новостным темам."
  "Базовый prompt для AI-анализа новостей."
  :type 'string :group 'про-новости)

(defcustom про-новости-максимум-заголовков 50
  "Максимальное число новостных заголовков, отправляемых за один запрос в AI."
  :type 'integer :group 'про-новости)

(defcustom про-новости-фильтр-рег nil
  "Регулярное выражение для фильтрации новостей; если nil, фильтрация по regexp не применяется."
  :type '(choice (const nil) regexp) :group 'про-новости)

(defcustom про-новости-исключить-список
  '("лайфхак" "спецпредложение" "скидки" "Clickbait")
  "Список стоп-слов: если заголовок содержит любое из этих слов, он исключается из выборки."
  :type '(repeat string) :group 'про-новости)

(defcustom про-новости-лог-dir
  (expand-file-name "про-новости-лог/" user-emacs-directory)
  "Директория для хранения логов аналитических сценариев новостей.
Каждый лог сохраняется в отдельном org-файле с подробной информацией о запросе и ответе AI."
  :type 'directory :group 'про-новости)

;; Локальная переменная для хранения контекста лога между отправкой и ответом AI.
(defvar-local про-новости--лог-контекст nil
  "Хранит meta информацию (заголовки, prompt, параметры) для последующего логирования AI-ответа.")

;;;; 1. Асинхронное получение новостей (обработка чанками)
;;
;; Функция получает все новости из базы elfeed за заданный промежуток времени (в секундах)
;; не блокируя интерфейс, а обрабатывая их по порциям (чанкам) заданного размера.
;; После разбивки на чанки вызывается process-chunk для каждой порции, а по завершению
;; вызывается callback с итоговым результатом.
;;
(defun про-новости--получить-асинхронно (секунд process-chunk callback)
  "Асинхронно получить новости за последние СЕКУНД.
PROCESS-CHUNK — функция для обработки порции новостей.
CALLBACK — функция, вызываемая по завершении обработки со списком результатов."
  (let* ((now (current-time))
         (since (time-subtract now (seconds-to-time секунд)))
         (all-entries (when (and (boundp 'elfeed-db-entries)
                                 (hash-table-p elfeed-db-entries))
                        (cl-remove-if-not
                         (lambda (entry)
                           (let ((pub (seconds-to-time (elfeed-entry-date entry))))
                             (and (time-less-p since pub) (time-less-p pub now))))
                         (hash-table-values elfeed-db-entries))))
         (chunk-size 200) ;; Размер одного чанка (можно изменить при необходимости)
         (head all-entries)
         (results '())
         (total (if all-entries (length all-entries) 0))
         (reported 0)
         progress)
    (if (not all-entries)
        (progn
          (when про-новости-отладка
            (message "[про-новости] База elfeed не инициализирована."))
          (message "Elfeed база новостей не инициализирована. Запустите M-x elfeed."))
      (setq progress (make-progress-reporter
                      (format "[про-новости] Сбор новостей: %d записей, чанки по %d" total chunk-size)
                      0 total))
      (cl-labels
          ((next-chunk ()
             (let ((chunk (cl-subseq head 0 (min chunk-size (length head)))))
               (setq head (nthcdr (length chunk) head))
               (setq results (append results (funcall process-chunk chunk)))
               (setq reported (+ reported (length chunk)))
               (progress-reporter-update progress reported)
               (if (and head (> (length head) 0))
                   (run-at-time 0 nil #'next-chunk)
                 (progn
                   (progress-reporter-done progress)
                   (funcall callback results))))))
        (next-chunk)))))

;;;; 2. Функции фильтрации, пакетирования и форматирования новостей
;;
;; Эти функции отвечают за:
;;  - Фильтрацию новостей по заданному регулярному выражению и списку стоп-слов.
;;  - Ограничение числа новостей (пакет) для передачи в AI.
;;  - Преобразование данных в удобочитаемый формат.
;;
(defun про-новости--фильтровать (lst &optional regexp exclude-list)
  "Отфильтровать LST новостей по REGEXP и списку исключений EXCLUDE-LIST.
Каждый элемент списка LST должен быть консом вида (заголовок . URL)."
  (let ((filtered (cl-remove-if
                   (lambda (x)
                     (or (and regexp (not (string-match-p regexp (car x))))
                         (cl-some (lambda (s)
                                      (string-match-p (regexp-quote s) (car x)))
                                    (or exclude-list про-новости-исключить-список))))
                   lst)))
    (when про-новости-отладка
      (message "[про-новости] Фильтрация: осталось %d из %d записей (regexp: %s)"
               (length filtered) (length lst) (or regexp "нет")))
    filtered))

(defun про-новости--пакет (lst &optional max)
  "Вернуть пакет новостей из списка LST, ограниченный числом MAX или
значением `про-новости-максимум-заголовков` по умолчанию."
  (let ((take (seq-take lst (or max про-новости-максимум-заголовков))))
    (when про-новости-отладка
      (message "[про-новости] Пакет новостей: выбрано %d записей" (length take)))
    take))

(defun про-новости--формат (lst &optional links)
  "Преобразовать список новостей LST в читаемую строку.
Если LINKS non-nil, для каждой новости указывается URL: '- Заголовок (URL)'."
  (mapconcat (lambda (x)
               (if links (format "- %s (%s)" (car x) (cdr x))
                 (format "- %s" (car x))))
             lst "\n"))

(defun про-новости--prompt (заголовки промпт &optional links)
  "Сформировать итоговый текст запроса для AI, используя ЗАГОЛОВКИ и ПРОМПТ.
Если LINKS non-nil, в итоговом тексте добавляются URL новостей."
  (let ((txt (string-join
              (list "Вот свежие новости."
                    (concat "Задание: " (or промпт про-новости-промт-по-умолчанию))
                    "---"
                    (про-новости--формат заголовки links))
              "\n\n")))
    (when про-новости-отладка
      (message "[про-новости] Сформирован prompt (начало): %s"
               (substring txt 0 (min 64 (length txt)))))
    txt))

;;;; 3. Макрос для создания интерактивных сценариев новостного дайджеста
;;
;; Этот макрос позволяет легко создавать интерактивные команды для отправки
;; новостного запроса в AI. Аргументы PLIST могут включать:
;;  :часов   — период, за который собираются новости (по умолчанию 24 часа);
;;  :максимум — максимальное число новостей;
;;  :промпт  — инструкция для AI (если не задан, используется DOCSTRING);
;;  :ссылки  — если non-nil, включать ссылки в формат;
;;  :фильтр — дополнительное регулярное выражение для фильтрации.
;;
(defmacro про-новости-сценарий (имя docstring &rest plist)
  "Создаёт интерактивную команду для AI-дайджеста новостей.
ИМЯ — символ команды.
DOCSTRING — строка документации (видна пользователю, не отправляется в AI).
PLIST может содержать такие ключи: :часов, :максимум, :промпт, :ссылки, :фильтр."
  (let ((часов    (plist-get plist :часов))
        (максимум (plist-get plist :максимум))
        (ссылки   (plist-get plist :ссылки))
        (фильтр   (plist-get plist :фильтр))
        (промпт   (plist-get plist :промпт)))
    `(defun ,имя ()
       ,docstring
       (interactive)
       (let* ((секунд (* 3600 ,(or часов 24)))
              (ai-промпт (or ,промпт ,docstring)))
         (message "[про-новости] Запуск фоновой сборки новостей...")
         (про-новости--получить-асинхронно
          секунд
          (lambda (chunk)
            ;; Преобразуем каждую новость в конс (заголовок . URL)
            (про-новости--фильтровать
             (mapcar (lambda (entry)
                       (cons (elfeed-entry-title entry)
                             (elfeed-entry-link entry)))
                     chunk)
             ,фильтр))
          (lambda (новости)
            (let ((итог (про-новости--пакет новости ,максимум)))
              (if (null итог)
                  (message "Нет подходящих новостей.")
                (про-новости-отправить-via-gptel итог ai-промпт ,ссылки)))))))))

;;;; 4. Логирование аналитических запросов в org-файл
;;
;; Функции логирования сохраняют в указанную директорию подробное содержание:
;; - Дата и время запроса.
;; - Использованный prompt.
;; - Заголовки новостей.
;; - Ответ от AI (при его получении).
;;
(defun про-новости--сделать-лог-файл (&optional сценарий)
  "Сгенерировать путь к лог-файлу для сценария СЦЕНАРИЙ или 'no-name'.
Файл создаётся в директории `про-новости-лог-dir`."
  (let* ((dir (file-name-as-directory про-новости-лог-dir))
         (scen (or сценарий "no-name"))
         (date (format-time-string "%Y-%m-%d"))
         (file (expand-file-name (format "%s-%s.org" scen date) dir)))
    (make-directory dir :parents)
    file))

(defun про-новости/логировать (заголовки промпт ответ &optional параметры)
  "Сохранить лог аналитического запроса.
ПРЕАМБУЛА: Заголовки, prompt, ответ и дополнительные параметры (plist) записываются в org-файл.
Возвращает путь к созданному лог-файлу."
  (message "[про-новости][логировать] Входные данные: заголовки=%S, prompt=%S, ответ=%S, параметры=%S"
           заголовки (if (> (length (or промпт "")) 80)
                         (substring промпт 0 80)
                       промпт)
           (if (> (length (or ответ "")) 80)
               (substring ответ 0 80)
             ответ)
           параметры)
  (let* ((файл (про-новости--сделать-лог-файл (plist-get параметры :сценарий)))
         (dt (format-time-string "%Y-%m-%d %H:%M"))
         (entry (concat
                 (format "* Аналитика [%s] :scenario_%s:\n" dt (or (plist-get параметры :сценарий) ""))
                 (when-let ((filt (plist-get параметры :фильтр)))
                   (format ":Фильтр: %s\n" filt))
                 (when-let ((mx (plist-get параметры :максимум)))
                   (format ":Максимум: %s\n" mx))
                 (when-let ((links (plist-get параметры :ссылки)))
                   (format ":Ссылки: %s\n" links))
                 (format ":Prompt:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n" (or промпт ""))
                 (format ":Заголовки:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                         (про-новости--формат заголовки t))
                 (format ":Ответ:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                         (or ответ "нет (ещё не получен)"))
                 "\n")))
    (with-temp-buffer
      (insert entry)
      (condition-case err
          (progn
            (append-to-file (point-min) (point-max) файл)
            (message "[про-новости][логировать] Лог успешно записан: %s" (abbreviate-file-name файл)))
        (error (message "[про-новости][логировать] Ошибка записи лога: %S" err))))
    файл))

;;;; 5. Отправка запроса в AI через gptel
;;
;; Функция создаёт новый чат-буфер, вставляет сформированный запрос и инициирует отправку
;; через gptel-send. Также сохраняется meta-контекст для последующего логирования ответа.
;;
(defun про-новости-отправить-via-gptel (заголовки промпт &optional links)
  "Отправить список новостей (ЗАГОЛОВКИ) и инструкцию (PROMПТ) в AI через gptel.
Если LINKS non-nil, в результирующем запросе включаются URL новостей."
  (message "[про-новости][отправить-via-gptel] Подготовка запроса с %d заголовками" (length заголовки))
  (let* ((bufname (format "*gptel-интернет-новости-%s*" (format-time-string "%Y%m%d-%H%M%S")))
         (txt (про-новости--prompt заголовки промпт links))
         (buf (gptel bufname))
         (meta (list :сценарий (when (and (boundp 'this-command)
                                          (symbolp this-command))
                                   (symbol-name this-command))
                      :фильтр про-новости-фильтр-рег
                      :максимум про-новости-максимум-заголовков
                      :ссылки links)))
    (pop-to-buffer buf)
    (with-current-buffer buf
      ;; Сохраняем meta-контекст для обработки ответа AI
      (setq-local про-новости--лог-контекст (list :заголовки заголовки :промпт промпт :meta meta))
      (goto-char (point-max))
      (insert txt "\n")
      (message "[про-новости][отправить-via-gptel] Запуск gptel-send в буфере %s" bufname)
      (let ((gptel-use-context nil))
        (gptel-send)))
    (message "[про-новости][отправить-via-gptel] Ожидайте ответа AI в новом буфере...")))

;;;; 6. Обработка ответа AI (hook)
;;
;; Хук, вызываемый после получения ответа от AI в буфере gptel.
;; Функция извлекает ответ, дополняет лог и запускает анализ новых тем.
;;
(defun про-новости--after-ai (beg end)
  "Обработчик после получения ответа AI.
BEG и END — позиции начала и конца ответа в буфере.
Логирует результаты и запускает ассистента для анализа новых тем."
  (let ((response (buffer-substring-no-properties beg end)))
    (message "[про-новости][after-ai] Получен ответ AI (начало): %s"
             (if (> (length response) 80) (substring response 0 80) response))
    (when (and (boundp 'про-новости--лог-контекст)
               про-новости--лог-контекст)
      (let* ((params про-новости--лог-контекст)
             (заголовки (plist-get params :заголовки))
             (промпт (plist-get params :промпт))
             (meta (plist-get params :meta))
             (ответ response))
        (message "[про-новости][after-ai] Подготовка логирования: заголовки=%S, prompt=%S, ответ=%s, meta=%S"
                 заголовки (if (> (length (or промпт "")) 80) (substring промпт 0 80) промпт)
                 (if (> (length (or ответ "")) 80) (substring ответ 0 80) ответ)
                 meta)
        (let ((log-file (про-новости/логировать заголовки промпт ответ meta)))
          (message "[про-новости][after-ai] Лог успешно записан: %S" log-file)
          ;; Запуск вспомогательных функций для анализа новых тем (ассистент внимательности)
          (про-новости-ассистент/инфо-месседж log-file 7)
          (setq-local про-новости--лог-контекст nil))))))

;; Обработка post-response AI: добавляем в список хуков gptel
(add-hook 'gptel-post-response-functions #'про-новости--after-ai)

;;;; 7. Дополнительные утилиты и функции управления историей
;;
;; Функции для:
;;  - Получения списка логов истории (с возможностью фильтрации по сценарию).
;;  - Выбора лог-файла для просмотра.
;;  - Открытия последних N логов.
;;  - Сравнения логов с помощью ediff.
;;  - Копирования дайджеста в kill-ring.
;;  - Предпросмотра новостей в echo-area.
;;
(defun про-новости-история/лог-файлы (&optional only-scen)
  "Получить список лог-файлов истории.
Если ONLY-SCEN non-nil, фильтрует по сценарию. Возвращает alist вида (\"СЦЕНАРИЙ | ДАТА\" . FILE)."
  (let* ((dir (file-name-as-directory про-новости-лог-dir))
         (files (when (file-directory-p dir)
                  (directory-files dir t "\\.org\\'"))))
    (cl-loop for f in files
             when (string-match "\\([^/]+\\)-\\([0-9]\\{4\\}-[0-9]\\+-[0-9]\\+\\)\\.org\\'" f)
             for scen = (match-string 1 f)
             for date = (match-string 2 f)
             if (or (not only-scen) (string= only-scen scen))
             collect (cons (format "%s | %s" scen date) f))))

(defun про-новости-история/выбрать ()
  "Интерактивно выбрать лог аналитики и открыть его."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (choice (completing-read "Выберите лог аналитики: " (mapcar #'car lst) nil t)))
    (when-let ((file (cdr (assoc choice lst))))
      (find-file file)
      (message "Открыт лог: %s" (abbreviate-file-name file)))))

(defun про-новости-история/последние (n)
  "Открыть последние N логов аналитики.
Логи сортируются по дате; каждый файл открывается в отдельном окне."
  (interactive "nСколько последних логов показать: ")
  (let* ((lst (sort (про-новости-история/лог-файлы)
                    (lambda (a b) (string> (cdr a) (cdr b)))))
         (sel (cl-subseq lst 0 (min n (length lst)))))
    (dolist (x sel)
      (find-file-other-window (cdr x))
      (message "Открыт лог: %s" (car x)))))

(defun про-новости-история/diff ()
  "Сравнить два выбранных лог-файла аналитики с помощью ediff."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (lst-names (mapcar #'car lst))
         (a (cdr (assoc (completing-read "Выберите старый лог: " lst-names nil t) lst)))
         (b (cdr (assoc (completing-read "Выберите свежий лог: " lst-names nil t) lst))))
    (when (and a b (file-exists-p a) (file-exists-p b))
      (ediff-files a b))))

(defun про-новости-копировать-дайджест (&optional часов максимум)
  "Скопировать в kill-ring дайджест свежих новостей со ссылками.
Параметры: ЧАСОВ (по умолчанию 24) и МАКСИМУМ новостей (по умолчанию 10)."
  (interactive)
  (let* ((ч (or часов 24))
         (m (or максимум 10))
         (сырой (про-новости--получить-асинхронно (* 3600 ч)
                                                          (lambda (chunk)
                                                            (mapcar (lambda (entry)
                                                                      (cons (elfeed-entry-title entry)
                                                                            (elfeed-entry-link entry)))
                                                                    chunk))
                                                          (lambda (новости) новости))) ;; Прямая выборка
         (итог (про-новости--пакет сырой m)))
    (if итог
        (progn
          (kill-new (про-новости--формат итог t))
          (message "Дайджест скопирован в kill-ring!"))
      (message "Нет данных для дайджеста."))))

(defun про-новости-превью (&optional часов максимум)
  "Показать краткий предпросмотр новостей в echo-area.
Использует новости за последние ЧАСОВ (по умолчанию 6) и МАКСИМУМ (по умолчанию 5)."
  (interactive)
  (let* ((n (or часов 6))
         (m (or максимум 5))
         (сырой (про-новости--получить-асинхронно (* 3600 n)
                                                          (lambda (chunk)
                                                            (mapcar (lambda (entry)
                                                                      (cons (elfeed-entry-title entry)
                                                                            (elfeed-entry-link entry)))
                                                                    chunk))
                                                          (lambda (новости) новости)))
         (итог (про-новости--пакет сырой m)))
    (if итог
        (message "\n%s" (про-новости--формат итог))
      (message "Нет свежих новостей для предпросмотра."))))

;;;; 8. Главное меню инструментов и сценариев
;;
;; Функция возвращает список всех доступных сценариев (интерактивных команд),
;; дополненный утилитами. Сценарии автоматически ищутся среди символов, начинающихся с "про-новости-".
;;
(defun про-новости/все-сценарии-menu-list ()
  "Возвращает список (label . function) всех интерактивных сценариев модуля.
Список включает пользовательские сценарии, утилиты и инструменты аналитики."
  (let* ((cmds '()))
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (string-match-p "\\`про-новости-" (symbol-name sym)))
         (let* ((name (symbol-name sym))
                (lbl (format "%s — %s" name (or (documentation sym t) "без описания")))
                (item (cons lbl sym)))
           (push item cmds))))
     obarray)
    (sort cmds (lambda (a b) (string< (car a) (car b))))))

(defun про-новости/меню ()
  "Показать главное меню модуля новостных сценариев и инструментов.
Позволяет выбрать и запустить любую интерактивную команду из списка."
  (interactive)
  (let* ((cmds (про-новости/все-сценарии-menu-list))
         (labels (mapcar #'car cmds))
         (choice (completing-read "Про-Новости: выберите сценарий → " labels nil t))
         (fun (cdr (assoc choice cmds))))
    (if fun
        (call-interactively fun)
      (message "Выбор не является командой."))))

;;;; Дополнительная справка (Eco-help)
;;
(defun про-новости/eco-help ()
  "Показать краткую справку и мотивационное сообщение для пользователей модуля."
  (interactive)
  (with-help-window "*про-новости Eco Help*"
    (princ
     "==== Emacs Про-Новости: Дао аналитики ====\n
• Модуль интегрирует сбор новостей, фильтрацию, отправку через AI и глубокий анализ.\n
• Добавляйте новые сценарии через макрос `про-новости-сценарий` – они появятся в меню автоматически.\n
• Используйте утилиты для просмотра, сравнения логов и семантического анализа тем.\n
• Отладочный режим включается переменной `про-новости-отладка`.\n
———
Бинды (пример):
  C-c n u   – Главное меню новостей
  C-c n h   – История логов\n
Пусть Дао новостей помогает вам анализировать информацию легко и без усилий.\n
--------------------------------------------------------
© П.К. / Emacs Дао\n")))

(provide 'про-новости)

;;; про-новости.el ends here
#+end_
;;; про-новости.el --- RSS, AI и Эстетика новостного дня для Emacs -*- lexical-binding: t -*-
;;
;; Автор: Пётр Косов <11111000000@email.com>
;;
;; Версия: 4.1
;; Keywords: news, RSS, Elfeed, AI, analytics, дайджест, эмаксовщина
;; URL: https://github.com/peterkosov/pro-news-ai
;;
;;; Commentary:
;;
;; Превратите Emacs в персональную редакцию новостей и аналитики для RSS/Elfeed:
;;
;;  - Ваши новости из Elfeed фильтруются интерактивно и отправляются прямо в нейросеть (GPTEL/AI) —
;;    для автоматического дайджеста, глубокого синопсиса, шуток, поэзии или строгой аналитики!
;;  - Макрос-сценарии позволяют создавать кастомные команды для любого типа выписки или анализа (от финансов
;;    до "юмористической подборки IT-новостей").
;;  - Структура гибкая: можно быстро задать временной диапазон, фильтр по темам, уникальный промпт.
;;  - Основные шаблоны вроде "Инфорадар", "Глубокие тренды", "Юмористика", "Q&A по новостям" уже встроены.
;;  - Можно копировать сгенерированный дайджест в kill-ring, устроить предпросмотр или сразу отправлять в AI!
;;
;; ------------------------------------------------------------------------------
;;   Быстрый старт:
;;     1. Убедитесь, что elfeed и gptel настроены.
;;     2. (require 'про-новости) в вашем init.
;;     3. Проверьте список доступных команд — ищите те, что начинаются на "про-новости-"!
;;
;; Рекомендуемая основная команда:
;;   M-x про-новости-выбрать-и-отправить
;;
;; Любую секцию можно использовать для создания новых макрос-сценариев под свои задачи.
;;
;; ------------------------------------------------------------------------------
;;; Code:

;;;; Зависимости и параметры

(require 'subr-x)
(require 'use-package)
(require 'cl-lib)
(require 'elfeed)
(require 'gptel)

(defgroup про-новости nil "RSS и AI-дайджест в Emacs." :group 'applications)

(defcustom про-новости-отладка t
  "Если t — выводить отладочную информацию (message) для всех процессов про-новости."
  :type 'boolean
  :group 'про-новости)

(defcustom про-новости-промт-по-умолчанию
  "Подытожь день: убери повторы и рекламу, дай аналитику новостных тем."
  "Базовый prompt для gptel анализа новостей."
  :type 'string :group 'про-новости)

(defcustom про-новости-максимум-заголовков 50
  "Максимум заголовков, отправляемых за раз в нейросеть."
  :type 'integer :group 'про-новости)

(defcustom про-новости-фильтр-рег nil
  "Регулярка-фильтр: только новости, что ей соответствуют. nil — все."
  :type '(choice (const nil) regexp) :group 'про-новости)

(defcustom про-новости-исключить-список
  '("лайфхак" "спецпредложение" "скидки" "Clickbait")
  "Исключать новости с этими словами в заголовке."
  :type '(repeat string) :group 'про-новости)

;;;; Асинхронный сбор и фильтрация новостей через chunk-и и таймер
;;
;; Не блокирует интерфейс даже при большом количестве записей Elfeed!
;;
;; Используйте для всех сценариев через про-новости--получить-асинхронно

(defun про-новости--получить-асинхронно (секунд process-chunk callback)
  "Асинхронно получить и обработать новости, опубликованные за последние СЕКУНД.
PROCESS-CHUNK — функция-обработчик порции новостей (list).
CALLBACK — финальный обработчик всего списка."
  (let* ((now (current-time))
         (since (time-subtract now (seconds-to-time секунд)))
         (all-entries
          (if (and (boundp 'elfeed-db-entries) (hash-table-p elfeed-db-entries))
              (cl-remove-if-not
               (lambda (entry)
                 (let ((pub (seconds-to-time (elfeed-entry-date entry))))
                   (and (time-less-p since pub) (time-less-p pub now))))
               (hash-table-values elfeed-db-entries))
            nil))
         (chunk-size 200) ;; размер одной порции обработки
         (head all-entries)
         (results '())
         (total (length all-entries))
         (reported 0)
         progress)
    (if (not all-entries)
        (progn
          (when про-новости-отладка
            (message "[про-новости] Elfeed база не инициализирована."))
          (message "Elfeed база новостей не инициализирована. Сначала запустите M-x elfeed."))
      (progn
        (setq progress (make-progress-reporter
                        (format "[про-новости] Чанк-сбор новостей: %d всего, по %d/шаг" total chunk-size)
                        0 total))
        ;; Внутренняя рекурсивная обработка чанков
        (cl-labels
            ((next-chunk ()
               (let ((chunk (cl-subseq head 0 (min chunk-size (length head)))))
                 (setq head (nthcdr (length chunk) head))
                 (setq results (append results (funcall process-chunk chunk)))
                 (setq reported (+ reported (length chunk)))
                 (progress-reporter-update progress reported)
                 (if (and head (> (length head) 0))
                     (run-at-time 0 nil #'next-chunk)
                   (progn
                     (progress-reporter-done progress)
                     (funcall callback results))))))
          (next-chunk))))))

;;;; Фильтрация: регулярка и стоп-слова

(defun про-новости--фильтровать (lst &optional regexp exclude-list)
  "Отфильтровать LST по REGEXP и EXCLUDE-LIST/глобальному списку."
  (let* ((filtered
          (cl-remove-if
           (lambda (x)
             (or (and regexp (not (string-match-p regexp (car x))))
                 (cl-some (lambda (s) (string-match-p (regexp-quote s) (car x)))
                          (or exclude-list про-новости-исключить-список))))
           lst)))
    (when про-новости-отладка
      (message "[про-новости] После фильтрации (%s): %d" 
               (or regexp "все") (length filtered)))
    filtered))

;;;; Сборка пакета и форматирование

(defun про-новости--пакет (lst &optional max)
  (let ((take (seq-take lst (or max про-новости-максимум-заголовков))))
    (when про-новости-отладка
      (message "[про-новости] Оставлено новостей: %d" (length take)))
    take))

(defun про-новости--формат (lst &optional links)
  "Человекочитаемый формат новостей: '- Заголовок (URL)'."
  (mapconcat (lambda (x)
               (if links (format "- %s (%s)" (car x) (cdr x))
                 (format "- %s" (car x))))
             lst "\n"))

(defun про-новости--prompt (заголовки промпт &optional links)
  (let ((txt (string-join
              (list
               "Вот свежие новости."
               (concat "Задание: " (or промпт про-новости-промт-по-умолчанию))
               "---"
               (про-новости--формат заголовки links))
              "\n\n")))
    (when про-новости-отладка
      (message "[про-новости] Сформирован запрос для GPTel: %s..."
               (substring txt 0 (min 64 (length txt)))))
    txt))

;;;; === Новый Главный Макрос-инструмент ===

(defmacro про-новости-сценарий (имя docstring &rest plist)
  "Создаёт интерактивную новостную команду для AI-дайджестов с неблокирующим сбором.
IMЯ — символ команды.
DOCSTRING — строка документации (user-visible, НЕ уходит в AI).
PLIST — :часов N, :максимум M, :промпт PROMPT, :ссылки T, :фильтр REGEXP.

Если :промпт не задан, используется docstring."
  (let ((часов    (plist-get plist :часов))
        (максимум (plist-get plist :максимум))
        (ссылки   (plist-get plist :ссылки))
        (фильтр   (plist-get plist :фильтр)))
    `(defun ,имя ()
       ,docstring
       (interactive)
       (let* ((секунд (* 3600 ,(or часов 24)))
              (промпт (or ,(plist-get plist :промпт) ,docstring)))
         (message "[про-новости] Фоновая сборка и фильтрация новостей...")
         (про-новости--получить-асинхронно
          секунд
          (lambda (chunk)
            ;; Формируем (заголовок . url) только из подошедших под фильтр
            (про-новости--фильтровать
             (mapcar (lambda (entry)
                       (cons (elfeed-entry-title entry)
                             (elfeed-entry-link entry)))
                     chunk)
             ,фильтр))
          (lambda (новости)
            (let ((итог (про-новости--пакет новости ,максимум)))
              (if (null итог)
                  (message "Нет подходящих новостей.")
                (про-новости-отправить-via-gptel итог промпт ,ссылки)))))))))

;;;; История аналитики: автоматическое логирование каждого сценария

(defcustom про-новости-лог-dir
  (expand-file-name "про-новости-лог/" user-emacs-directory)
  "Папка для хранения логов аналитики новостных сценариев.
Каждый срез сохраняется в отдельный org-файл (или plain), с промптом и основными параметрами."
  :type 'directory
  :group 'про-новости)

(defun про-новости--сделать-лог-файл (&optional сценарий)
  "Сгенерировать путь к лог-файлу для текущего сценария/дня."
  (let* ((dir (file-name-as-directory про-новости-лог-dir))
         (scen (or сценарий "no-name"))
         (date (format-time-string "%Y-%m-%d"))
         (file (expand-file-name (format "%s-%s.org" scen date) dir)))
    (make-directory dir :parents)
    file))

(defun про-новости/логировать (заголовки промпт ответ &optional параметры)
  "Записать факт аналитики (заголовки, промпт, ответ, meta) в файл истории.
Параметры (plists): :сценарий, :время, :фильтр, :максимум, :ссылки, и др."
  (message "[про-новости][логировать] ВХОД: %S %S %S %S" заголовки промпт (if (> (length (or ответ "")) 80) (substring ответ 0 80) ответ) параметры)
  (let* ((файл (про-новости--сделать-лог-файл (plist-get параметры :сценарий)))
         (dt (format-time-string "%Y-%m-%d %H:%M"))
         (entry
           (concat
            (format "* Аналитика [%s] :scenario_%s:\n" dt (or (plist-get параметры :сценарий) ""))
            (when-let ((filt (plist-get параметры :фильтр)))
              (format ":Фильтр: %s\n" filt))
            (when-let ((mx (plist-get параметры :максимум)))
              (format ":Максимум: %s\n" mx))
            (when-let ((links (plist-get параметры :ссылки)))
              (format ":Ссылки: %s\n" links))
            (format ":Prompt:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n" (or промпт ""))
            (format ":Заголовки:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                    (про-новости--формат заголовки t))
            (format ":Ответ:\n#+BEGIN_EXAMPLE\n%s\n#+END_EXAMPLE\n"
                    (or ответ "нет (ещё не получен)"))
            "\n")))
    (message "[про-новости][логировать] Попытка записи в файл: %s" файл)
    (with-temp-buffer
      (insert entry)
      (condition-case err
          (progn
            (append-to-file (point-min) (point-max) файл)
            (message "[про-новости][логировать] УСПЕШНО записан лог: %s" (abbreviate-file-name файл)))
        (error (message "[про-новости][логировать] Ошибка записи файла: %S" err))))
    файл))

;;;; Отправка в gptel (замена chatgpt-shell)

(defvar-local про-новости--лог-контекст nil
  "Хранит meta информацию для передачи в лог при получении ответа от AI.")

(defun про-новости-отправить-via-gptel (заголовки промпт &optional links)
  "Создаёт чат-буфер, асинхронно отправляет в AI, потом логирует результат.
Все параметры сохраняются в log истории (org-файл)."
  (message "[про-новости][отправить-via-gptel] ВХОД: заголовки=%S, промпт=%S, links=%S" заголовки (if (> (length (or промпт "")) 80) (substring промпт 0 80) промпт) links)
  (let* ((bufname (format "*gptel-интернет-новости-%s*"
                          (format-time-string "%Y%m%d-%H%M%S")))
         (txt (про-новости--prompt заголовки промпт links))
         (buf (gptel bufname))
         (meta (list
                :сценарий (when (and (boundp 'this-command)
                                     (symbolp this-command)
                                     this-command)
                             (symbol-name this-command))
                :фильтр про-новости-фильтр-рег
                :максимум про-новости-максимум-заголовков
                :ссылки links)))
    (message "[про-новости][отправить-via-gptel] Новый буфер: %s, meta=%S" bufname meta)
    (pop-to-buffer buf)
    (with-current-buffer buf
      ;; <---- Сохраняем meta, чтобы дополнить log после AI-ответа
      (setq-local про-новости--лог-контекст
                  (list :заголовки заголовки :промпт промпт :meta meta))
      (goto-char (point-max))
      (insert txt "\n")
      (message "[про-новости][отправить-via-gptel] Запуск gptel-send (buf=%s)" bufname)
      (let ((gptel-use-context nil))
        (gptel-send)))
    (message "[про-новости][отправить-via-gptel] Ждите ответа от ИИ в новом буфере...")))

;; --- Логирование результата AI-процесса (hook в gptel-post-response) ---
;;; --------------------------------------------------------------------------
;;; Ассистент внимательности (метанаблюдатель) — автоматическое замечание новых тем/слов

(defun про-новости--log-get-words (log-file)
  "Извлечь список уникальных слов из LOG-FILE для мета-анализа."
  (when (and log-file (file-readable-p log-file))
    (with-temp-buffer
      (insert-file-contents log-file)
      (let* ((txt (buffer-string))
             (words (split-string 
                     (downcase
                      (replace-regexp-in-string "[^a-zа-яё0-9_-]+" " " txt))
                     " +" t)))
        (delete-dups (cl-remove-if (lambda (w) (< (length w) 4)) words))))))

(defun про-новости--log-recent-files (n &optional сценарий)
  "Получить список последних N лог-файлов истории, можно фильтровать по СЦЕНАРИЙ."
  (let* ((lst (про-новости-история/лог-файлы сценарий))
         (sorted (sort lst (lambda (a b) (string> (cdr a) (cdr b))))))
    (mapcar #'cdr (cl-subseq sorted 0 (min n (length sorted))))))

(defun про-новости-ассистент/проанализировать-новое (&optional current-log n-compare)
  "Сравнить CURRENT-LOG (или последний) с предыдущими N-COMPARE (по умолчанию 7).
Возвращает список новых слов/тем, которых не было раньше (с удалением стоп-слов)."
  (let* ((cur-log (or current-log
                      (car (про-новости--log-recent-files 1))))
         (prev-logs (cdr (про-новости--log-recent-files (or n-compare 8))))
         (cur-words (про-новости--log-get-words cur-log))
         (prev-words (delete-dups (apply #'append
                                         (mapcar #'про-новости--log-get-words prev-logs))))
         (stop '( "https" "http" "2024" "2023" "2022" "вест" "новост" "замеч" "росси" "ссыл"
                   "главн" "дата" "интернет" "анализ" "ответ" "лог" "дайджест" "инфо" "тема"))
         (new-words (cl-set-difference cur-words prev-words :test #'equal)))
    (cl-remove-if (lambda (w)
                    (or (member w stop)
                        (string-match-p "^[0-9]+$" w)
                        (< (length w) 4)))
                  new-words)))

(defun про-новости-ассистент/инфо-месседж (&optional cur-log n-compare)
  "Если появились новые темы — уведомляет пользователя в minibuffer."
  (let* ((новые (про-новости-ассистент/проанализировать-новое cur-log n-compare)))
    (when (and новые (> (length новые) 0))
      (message "‼️ Ассистент внимательности: Обнаружены новые темы! → %s"
               (string-join (seq-take новые 6) ", "))
      новые)))

(defun про-новости-ассистент/ai-отчет (&optional cur-log n-compare)
  "Генерирует отчёт и отправляет в gptel — что появилось нового, каков тренд."
  (interactive)
  (let* ((recent (про-новости--log-recent-files (or n-compare 8)))
         (cur-log (or cur-log (car recent)))
         (prev-logs (cdr recent)))
    (if (not cur-log)
        (message "Нет ни одного лога истории для AI-отчёта — накопите хоть один log!")
      (let* ((старое (if prev-logs
                         (mapconcat (lambda (f) (with-temp-buffer
                                                 (insert-file-contents f)
                                                 (buffer-string)))
                                    prev-logs "\n\n---\n\n")
                       "нет данных"))
             (новое (if (and cur-log (file-readable-p cur-log))
                        (with-temp-buffer
                          (insert-file-contents cur-log)
                          (buffer-string))
                      "нет свежих логов"))
             (промпт "Сравни последние свежие новости и аналитику с предыдущей неделей. Выдели новые слова, темы и лаконично опиши новые тренды и риски."))
        (про-новости-отправить-via-gptel
         `(("Новая аналитика" . "")
           ("Старые отчеты:" . ""))
         (concat промпт
                 "\n---\nСвежий лог:\n" новое
                 "\n---\nЛоги прошлых дней:\n" старое))))))

;;; --- Интеграция с post-response логгером ---

(defun про-новости--after-ai (beg end)
  "Логирует ответ в историю после получения отклика AI и запускает ассистента внимательности.
BEG и END — позиции ответа в буфере."
  (let ((response (buffer-substring-no-properties beg end)))
    (message "[про-новости][after-ai] Вызвана post-response-функция: response (head):%s, beg=%S end=%S"
             (if (> (length response) 80) (substring response 0 80) response)
             beg end)
    (when (and (boundp 'про-новости--лог-контекст)
               про-новости--лог-контекст)
      (let* ((params про-новости--лог-контекст)
             (заголовки (plist-get params :заголовки))
             (промпт (plist-get params :промпт))
             (meta (plist-get params :meta))
             (ответ response))
        (message "[про-новости][after-ai] Готов логировать: headers=%S, prompt=%S, ответ head=%s, meta=%S"
                 заголовки (if (> (length (or промпт "")) 80) (substring промпт 0 80) промпт)
                 (if (> (length (or ответ "")) 80) (substring ответ 0 80) ответ)
                 meta)
        (let ((log-file (про-новости/логировать заголовки промпт ответ meta)))
          (message "[про-новости][after-ai] Лог-файл создан: %S" log-file)
          (про-новости-ассистент/инфо-месседж log-file 7)
          (setq-local про-новости--лог-контекст nil)))))) 


(add-hook 'gptel-post-response-functions #'про-новости--after-ai)

;;; ---------------------
;;; Меню: ассистент внимательности

(defun про-новости/меню-ассистент ()
  "Меню анализа новых тем: ассистент внимательности."
  (interactive)
  (let* ((opts '("Показать новые слова/темы (за неделю)"
                 "AI-отчет по свежим отличиям"
                 "Выход"))
         (choice (completing-read "Ассистент внимательности — что сделать? " opts nil t)))
    (cond
     ((string= choice "Показать новые слова/темы (за неделю)")
      (let ((lst (про-новости-ассистент/проанализировать-новое nil 7)))
        (if lst
            (message "‼️ Новые темы: %s" (string-join lst ", "))
          (message "Нет новых тем!"))))
     ((string= choice "AI-отчет по свежим отличиям")
      (про-новости-ассистент/ai-отчет))
     (t (message "Отменено.")))))

(defun про-новости/меню ()
  "Главное меню: простое, надёжное, состоит только из реальных интерактивных сценариев."
  (interactive)
  (let* ((cmds (про-новости/все-сценарии-menu-list))
         (labels (mapcar #'car cmds))
         (choice (completing-read "Про-Новости: выберите сценарий → " labels nil t))
         (fun (cdr (assoc choice cmds))))
    (if fun
        (call-interactively fun)
      (message "⏸️ Это не команда, а раздел меню."))))



;;;; ───────────────────────────────
;;;; Готовые и продвинутые сценарии (макрокоманды на один клик)
;;;; ───────────────────────────────

;; Всё, что ниже — реальные AI-сценарии! Используйте команду или сделайте свой clone.

(про-новости-сценарий
 про-новости-за-час
  "Быстрый дайджест последних новостей за 1 час (до 25 штук)."
  :часов 1 :максимум 25
  :промпт "Кратко изложи всё по темам. Если есть весёлое — отметь. Если чувствуется тренд — кратко подчеркни.")

(про-новости-сценарий
 про-новости-за-день
  "Классический новостной обзор за 12 часов (до 40)."
  :часов 12 :максимум 40
  :промпт "Сделай обзор без воды, без дублей — только важные и новые события. Краткая структура, только суть и общий вывод.")

(про-новости-сценарий
 про-новости-финансы
  "Все свежие финансовые новости за сутки."
  :часов 24 :максимум 30
  :промпт "Сконцентрируйся только на крупных событиях финансов, банков, бирж, крипто и макроэкономики. Изложи строго, без лишнего."
  :фильтр "финанс\\|банк\\|бирж\\|эконом\\|крипт")

(про-новости-сценарий
 про-новости-гик
  "Айти/наука/технологии — полный гик-дайджест за 24ч."
  :часов 24 :максимум 40 :ссылки t
  :промпт "Выбери только научные, IT, технологии или Linux/AI новости. Если есть значимые анонсы/софт — выдели отдельно."
  :фильтр "ИИ\\|AI\\|технол\\|наук\\|разработк\\|Linux\\|алгоритм\\|гаджет")

(про-новости-сценарий
 про-новости-кратко
  "AI-компакт: топ-5 событий дня — сразу структурировано."
  :часов 24 :максимум 30
  :промпт "Кратко и по структуре: выбери 4-6 главных события дня, на каждое дай 1-2 предложения пояснения. Итоговый вывод — тренд дня.")

;; ——— Новое: "ИНФОРМАЦИОННЫЙ РАДАР" — тем/трендов ———
(про-новости-сценарий
 про-новости-темы-радара
  "Отчёт по горячим темам и инфоповодам в RSS за 24 часа."
  :часов 24 :максимум 60
  :промпт "Выдели все повторяющиеся или часто встречающиеся темы, упорядочи их по частоте. Покажи по каждой теме — коротко суть, кто участвует, почему это важно.")

;; ——— Ещё: Q&A Генератор на основе новостных лент ———
(про-новости-сценарий
 про-новости-вопросы-ответы
  "Автоматически сгенерируй список вопросов и ответов по новостям суток."
  :часов 24 :максимум 30
  :промпт "Для каждой новости сформулируй адекватный вопрос (что, кто, почему). Сразу дай краткий ответ, основываясь на заголовке и тренде. Кратко и по делу.")

;; ——— Юмор: Генератор веселого AI-дайджеста ———
(про-новости-сценарий
 про-новости-юмор
  "Попробуй обернуть события дня в ироничный или юмористический дайджест."
  :часов 24 :максимум 30
  :промпт "Составь юмористический дайджест: перескажи новости дня с лёгкой иронией, подбрось собственный смешной вывод или мем, но не теряй сути событий.")

;; ——— Глубокий анализ: резюмируй скрытые тренды и связи ———
(про-новости-сценарий
 про-новости-глубоко
  "AI-аналитика: дай неочевидные связи и прогнозы на основе последних 2 суток."
  :часов 48 :максимум 60
  :промпт "Проанализируй полученные новости: найди скрытые тренды, повторяющиеся темы, неочевидные зависимости и взаимосвязи между событиями. Кратко спрогнозируй как это может повлиять на развитие тем в ближайшие дни/недели.")

;;;; Добавляйте свои сценарии по желанию — просто скопируйте макрос выше!

;;;; Мега-универсальная команда (ввод фильтра, промпта, времени)

(defun про-новости-выбрать-и-отправить (часов regexp максимум промпт links)
  "Асинхронно собрать дайджест новостей и отправить в gptel.
 - ЧАСОВ назад,
 - REGEXP (строка или \"\" = все),
 - МАКСИМУМ,
 - ПРОМПТ (AI-инструкция),
 - LINKS (ссылки или нет)."
  (interactive
   (list (read-number "Часов назад: " 24)
         (read-string "Фильтр (regexp, пусто = все): " "")
         (read-number "Максимум новостей: " 30)
         (read-string "Промпт для AI (Enter — стандарт): ")
         (y-or-n-p "Включить ссылки? ")))
  (let* ((секунд (* 3600 часов))
         (фильт (if (string-empty-p regexp) nil regexp))
         (ai-промпт (unless (string-empty-p промпт) промпт)))
    (message "[про-новости] Фоновая сборка и фильтрация...")
    (про-новости--получить-асинхронно
     секунд
     (lambda (chunk)
       ;; фильтрация по regexp/исключениям
       (про-новости--фильтровать
        (mapcar (lambda (entry)
                  (cons (elfeed-entry-title entry)
                        (elfeed-entry-link entry)))
                chunk)
        фильт))
     (lambda (новости)
       (let ((итог (про-новости--пакет новости максимум)))
         (if (null итог)
             (message "Нет подходящих новостей.")
           (про-новости-отправить-via-gptel итог ai-промпт links)))))))

;;;; == Просмотр и сравнение Истории Аналитики ==

(defun про-новости-история/лог-файлы (&optional only-scen)
  "Список всех файлов логов истории (можно фильтровать сценарием).
Возвращает alist: (\"СЦЕНАРИЙ | ДАТА\" . file)."
  (let* ((dir (file-name-as-directory про-новости-лог-dir))
         (files (when (file-directory-p dir)
                  (directory-files dir t "\\.org\\'"))))
    (cl-loop for f in files
             when (string-match "\\([^/]+\\)-\\([0-9][0-9][0-9][0-9]-[0-9]+-[0-9]+\\)\\.org\\'" f)
             for scen = (match-string 1 f)
             for date = (match-string 2 f)
             if (or (not only-scen) (string= only-scen scen))
             collect (cons (format "%s | %s" scen date) f))))

(defun про-новости-история/выбрать ()
  "Интерактивно выбрать и открыть файл истории анализа.
Показывает список всех срезов, при выборе открывает в новом буфере."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (choice (completing-read "Выберите лог аналитики: " (mapcar #'car lst) nil t)))
    (when-let ((file (cdr (assoc choice lst))))
      (find-file file)
      (message "Открыт: %s" (abbreviate-file-name file)))))
      
(defun про-новости-история/последние (n)
  "Открыть последний N логов истории (читает файлы логов, сортирует по дате/имени)."
  (interactive "nСколько последних логов показать: ")
  (let* ((lst (sort (про-новости-история/лог-файлы)
                    (lambda (a b) (string> (cdr a) (cdr b)))))
         (sel (cl-subseq lst 0 (min n (length lst)))))
    (dolist (x sel)
      (find-file-other-window (cdr x))
      (message "Открыт: %s" (car x)))))
    
(defun про-новости-история/diff ()
  "Сравнить два лога аналитики (выбрать из истории, открыть визуальный diff)."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (lst-names (mapcar #'car lst))
         (a (cdr (assoc (completing-read "Первый лог (старый): " lst-names nil t) lst)))
         (b (cdr (assoc (completing-read "Второй лог (свежий): " lst-names nil t) lst))))
    (when (and a b (file-exists-p a) (file-exists-p b))
      (ediff-files a b))))

;;; --- Карта со-встречающихся тем (semantic network) ---

(defun про-новости--извлечь-топ-темы (log-file &optional top-n)
  "Извлечь TOP-N частых словосочетаний (тем) из лог-файла LOG-FILE."
  (when (and log-file (file-readable-p log-file))
    (with-temp-buffer
      (insert-file-contents log-file)
      (let* ((txt (buffer-string))
             ;; Упрощённый n-грам извлекатор (можно развить)
             (words (split-string
                     (downcase (replace-regexp-in-string "[^a-zа-яё0-9_-]+" " " txt))
                     " +" t))
             (bigrams (cl-loop for (a b) on words
                               when b collect (format "%s %s" a b)))
             (freq (make-hash-table :test #'equal)))
        (dolist (bg bigrams)
          (puthash bg (1+ (gethash bg freq 0)) freq))
        (mapcar #'car
                (seq-take (sort (hash-table-keys freq)
                                (lambda (x y)
                                  (> (gethash x freq 0) (gethash y freq 0))))
                          (or top-n 15)))))))

(defun про-новости-связи/построить-карту (log-files &optional minfreq)
  "Строит карту тем: (тема . (связанные темы частота ...)), анализируя LOG-FILES."
  (let ((net (make-hash-table :test #'equal))
        (all-bigrams '()))
    (dolist (f log-files)
      (let ((bigrams (про-новости--извлечь-топ-темы f 50)))
        (setq all-bigrams (append all-bigrams bigrams))))
    (dolist (bg all-bigrams)
      (let ((pair (split-string bg " " t)))
        (when (= (length pair) 2)
          (let ((a (car pair)) (b (cadr pair)))
            (puthash a (cons b (gethash a net)) net)))))
    ;; оставить только те связи, которые встречались чаще minfreq
    (let ((result '()))
      (maphash (lambda (k v)
                 (let* ((c (cl-count-if (lambda (x) (member x v)) v))
                        (uniq (delete-dups v)))
                   (when (or (null minfreq) (> (length uniq) (or minfreq 1)))
                     (push (cons k uniq) result))))
               net)
      result)))

(defun про-новости-связи/org-представление (map)
  "Преобразовать карту тем (MAP, alist) в org-table для вставки/экспорта."
  (let ((lines '("| Тема | Связанные темы |")))
    (dolist (cell map)
      (push (format "| %s | %s |"
                    (car cell)
                    (string-join (cl-subseq (cdr cell) 0 (min 6 (length (cdr cell)))) ", "))
            lines))
    (string-join (nreverse lines) "\n")))

(defun про-новости-связи/показать-карту (&optional days)
  "Показать карту тем и связей за DAYS дней (default 7)."
  (interactive "p")
  (let* ((log-files (про-новости--log-recent-files (or days 7)))
         (map (про-новости-связи/построить-карту log-files 2))
         (tbl (про-новости-связи/org-представление map)))
    (with-current-buffer (get-buffer-create "*Карта тем (про-новости)*")
      (read-only-mode -1) (erase-buffer)
      (insert "#+TITLE: Карта тем за последние дни\n\n")
      (insert tbl)
      (org-mode) (goto-char (point-min)) (org-table-align)
      (pop-to-buffer (current-buffer)))))

(defun про-новости-связи/ai-инсайт (&optional days)
  "Отправить в AI карту тем за DAYS дней для deep-insight."
  (interactive "p")
  (let* ((log-files (про-новости--log-recent-files (or days 7)))
         (map (про-новости-связи/построить-карту log-files 2))
         (tbl (про-новости-связи/org-представление map))
         (prompt "Проанализируй semantic network и карту тем (org-table). Дай инсайт: какие темы и связи сейчас наиболее значимы, какой неожиданный кластер возникает, что связывает новые и уходящие темы."))
    (про-новости-отправить-via-gptel
     `(("Карта тем" . "") ("org-table:" . ,tbl))
     prompt)))

;; Пункты 'Карты тем' и 'AI-инсайт' теперь всегда есть в главном меню (см. ниже).
;; Расширять список легко — просто добавь нужный пункт в sections.

(defun про-новости-история/искать (regexp)
  "Найти все логи, где встречается REGEXP (искать по содержимому файла)."
  (interactive "sПоиск в истории (REGEXP): ")
  (let* ((lst (про-новости-история/лог-файлы))
         (hits (cl-remove-if-not
                (lambda (cell)
                  (with-temp-buffer
                    (insert-file-contents (cdr cell))
                    (re-search-forward regexp nil t)))
                lst)))
    (if hits
        (let ((choice (completing-read "Где найдено: " (mapcar #'car hits) nil t)))
          (find-file (cdr (assoc choice hits)))
          (message "Открыт: %s" choice))
      (message "Ничего не найдено по \"%s\"." regexp))))

(defun про-новости-история/копировать-в-буфер ()
  "Копировать весь лог истории (выбор) в kill-ring — для вставки в org или заметки."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (choice (completing-read "Лог для копирования: " (mapcar #'car lst) nil t))
         (file (cdr (assoc choice lst))))
    (when (and file (file-exists-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (kill-new (buffer-string)))
      (message "Лог скопирован в kill-ring: %s" choice))))

;;;; == Меню управления аналитикой и сценариями (единое для всех функций) ==
(defun про-новости/все-сценарии-menu-list ()
  "Возвращает минималистичный список команд с иконками: (label . function).
Включает все AI-сценарии и полезные утилиты (карта, diff и др) с их иконками."
  (let* ((main-icons
          '(("про-новости-за-час"         . "⭐")
            ("про-новости-за-день"        . "⭐")
            ("про-новости-гик"            . "⭐")
            ("про-новости-кратко"         . "⭐")
            ("про-новости-финансы"        . "📰")
            ("про-новости-глубоко"        . "🧠")
            ("про-новости-юмор"           . "🤣")
            ("про-новости-темы-радара"    . "🌐")
            ("про-новости-вопросы-ответы" . "❓")
            ("про-новости/меню-ассистент" . "🚦")
            ;; Системные и служебные
            ("про-новости-история/выбрать"   . "📜")
            ("про-новости-история/последние" . "📜")
            ("про-новости-история/diff"      . "📊")
            ("про-новости-история/искать"    . "🔎")
            ("про-новости-история/копировать-в-буфер" . "📋")
            ("про-новости-связи/показать-карту"       . "🌐")
            ("про-новости-связи/ai-инсайт"            . "🧠")
            ("про-новости-превью"                     . "⏱️")
            ("про-новости-копировать-дайджест"        . "⏱️")
            ("про-новости/eco-help"                   . "❔")
            ))
         (fixed-menu
          `(
            ("📜 История аналитики: выбрать запись"    . про-новости-история/выбрать)
            ("📜 Последние N логов (несколько окон)"   . про-новости-история/последние)
            ("📊 Сравнить два среза (diff)"            . про-новости-история/diff)
            ("🔎 Искать в истории"                     . про-новости-история/искать)
            ("📋 Копировать лог для заметок"           . про-новости-история/копировать-в-буфер)
            ("🌐 Карта тем/связей (semantic net)"      . про-новости-связи/показать-карту)
            ("🧠 AI-инсайт по теме-кластеризации"      . про-новости-связи/ai-инсайт)
            ("🚦 Ассистент внимательности (новые темы/слова)" . про-новости/меню-ассистент)
            ("⏱️ Превью свежих новостей (echo-area)"   . про-новости-превью)
            ("⏱️ Копировать дайджест (kill-ring)"      . про-новости-копировать-дайджест)
            ("🧩 Перезагрузить пользовательские сценарии". про-новости/загрузить-все-сценарии)
            ("❔ Eco-help / Справка"                   . про-новости/eco-help)
           ))
         (cmds '()))
    ;; Сценарии (авто из функций)
    (mapatoms
     (lambda (sym)
       (when (and (commandp sym)
                  (string-match "\\`про-новости-" (symbol-name sym)))
         (let* ((name (symbol-name sym))
                (icon (or (cdr (assoc name main-icons)) "📰"))
                (doc (or (documentation sym t) "нет описания"))
                (base (replace-regexp-in-string
                       "-" " "
                       (string-remove-prefix "про-новости-" name)))
                (lbl (format "%s %s — %s" icon base (replace-regexp-in-string "\n.*" "" doc))))
           (push (cons lbl sym) cmds))))
     obarray)
    ;; Добавляем утилиты и фиксы с иконками, если они ещё не в списке
    (dolist (item fixed-menu)
      (let* ((fun (cdr item))
             (p (assoc fun (mapcar (lambda (c) (cons (cdr c) (car c))) cmds))))
        (unless p
          (let* ((name (if (symbolp fun) (symbol-name fun) ""))
                 (icon (or (cdr (assoc name main-icons))
                           (and (string-match "^про-новости-связи/" name) "🌐")
                           (and (string-match "^про-новости-история/" name) "📜")
                           "🧰"))
                 (lbl (format "%s %s" icon (car item))))
            (push (cons lbl fun) cmds)))))
    (sort cmds (lambda (a b) (string< (car a) (car b))))))

(defun про-новости/eco-help ()
  "Показать даосскую справку/мотивацию."
  (interactive)
  (with-help-window "*про-новости Eco Help*"
    (princ
     "==== Emacs Про-Новости: Дао Аналитики ====\n
• Один модуль — неограниченный поток: сценарии растут сами, без хаоса.
• Экспертные и кастомные сценарии (user-файлы) — появляются автоматически!
• Навигация по истории, глубокие сравнения, semantic network, внимательность — всё сразу.
———
🆘 Бинды (пример): C-c n u — универсальное меню: сценарии, история, AI-tools, админ.
———
Совет: просто добавьте новый (про-новости-сценарий ...) — он тут же будет в меню.
Пусть Новостное Дао работает за вас, а не против вас!
--------------------------------------------------------
© П.К. / путь Дао Emacs\n")))


;;;; == Система пользовательских сценариев-расширений ==
;; Путь Дао: пусть сценарии растут как бамбук, из той почвы, на которой нужнее.

(defcustom про-новости-доп-скрипты-dir
  (expand-file-name "про-новости-сценарии/" user-emacs-directory)
  "Каталог с пользовательскими сценариями для новостной аналитики.
Все .el файлы из этой папки будут автоматически загружаться при вызове специальной функции."
  :type 'directory
  :group 'про-новости)

(defun про-новости/загрузить-все-сценарии ()
  "Загружает все пользовательские сценарии из `про-новости-доп-скрипты-dir`.
Это позволяет расширять мод новыми сценариями без необходимости менять основной файл.
Создайте свои (про-новости-сценарий ...) — и просто положите файл-функцию в эту папку!"
  (interactive)
  (let ((dir (file-name-as-directory про-новости-доп-скрипты-dir)))
    (when (file-directory-p dir)
      (dolist (f (directory-files dir t "\\.el\\'"))
        (load-file f)
        (when про-новости-отладка
          (message "[про-новости] Загружен пользовательский сценарий: %s" f))))
    (when про-новости-отладка
      (message "[про-новости] Все пользовательские сценарии подхвачены!"))))

;; Типичный способ: (add-hook 'emacs-startup-hook #'про-новости/загрузить-все-сценарии)
;; или вызвать вручную перед работой с новостями.

;;; Дао-совет:
;; Пусть каждый напишет себя свой (про-новости-сценарий имя ...), просто скопировав шаблон,
;; и ваш инструментарий аналитики будет мягко разрастаться по мере реальных задач!

;;;; == Полезные вспомогательные функции ==

(defun про-новости-копировать-дайджест (&optional часов максимум)
  "В kill-ring — список свежих главных новостей со ссылками."
  (interactive)
  (let* ((ч (or часов 24)) (m (or максимум 10))
         (сырой (про-новости--получить (* 3600 ч)))
         (итог (про-новости--пакет сырой m)))
    (if итог
        (progn (kill-new (про-новости--формат итог t))
               (message "Дайджест скопирован!"))
      (message "Данных нет."))))

(defun про-новости-превью (&optional часов максимум)
  "Показ топ-новостей в echo-area: быстро и лаконично."
  (interactive)
  (let* ((n (or часов 6)) (m (or максимум 5))
         (сырой (про-новости--получить (* 3600 n)))
         (итог (про-новости--пакет сырой m)))
    (if итог
        (message "\n%s" (про-новости--формат итог))
      (message "Нет свежих новостей!"))))

;;;; ────────────────────────────────
;;;; Примеры биндов (горячих клавиш)
;;;; ────────────────────────────────
;; Настройте свои любимые бинды (пример):
;;
;; (global-set-key (kbd "C-c n u") #'про-новости-выбрать-и-отправить)
;; (global-set-key (kbd "C-c n h") #'про-новости-юмор)
;; (global-set-key (kbd "C-c n g") #'про-новости-гик)
;; (global-set-key (kbd "C-c n f") #'про-новости-финансы)
;; (global-set-key (kbd "C-c n t") #'про-новости-темы-радара)
;; (global-set-key (kbd "C-c n d") #'про-новости-за-день)
;; (global-set-key (kbd "C-c n k") #'про-новости-кратко)
;; (global-set-key (kbd "C-c n s") #'про-новости-глубоко)
;; (global-set-key (kbd "C-c n q") #'про-новости-вопросы-ответы)
;; (global-set-key (kbd "C-c n c") #'про-новости-копировать-дайджест)
;;
;; Любой сценарий всегда можно вызывать и через M-x, все команды начинаются с "про-новости-"!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 🌅 Думайте гибко: любые новостные выпуски вы делаете собственными шаблонами.
;;;   Пишите один шаблон (макрос) — и кайфуйте от сквозной автоматизации новостной аналитики!
;;
;;; — П.К.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'про-новости)
;;; про-новости.el ends here
