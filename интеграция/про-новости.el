;;; про-новости.el --- Асинхронные дайджесты и AI-аналитика для новостей на Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Модуль “Про-Новости” — ваш быстрый, умный помощник по дайджестам и аналитике
;; новостных лент прямо в Emacs.
;;
;; Возможности:
;;  - Асинхронный сбор новостей (без блокировки интерфейса)
;;  - Готовые макросценарии для быстрого анализа по тематикам и временным срезам
;;  - Интеграция с gptel (AI): пояснения, анализ трендов, креативные отчёты
;;  - История аналитики: автоматизация логирования, сравнение периодов, поиски тем, карты тем
;;  - “Ассистент внимательности” — отслеживание новых тем и слов
;;  - Меню быстрых запусков: все сценарии и анализ доступны в пару кликов!
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)

(use-package elfeed
  :ensure t)

(use-package gptel
  :ensure t)

(defgroup про-новости nil
  "Асинхронные дайджесты и AI-аналитика для новостных лент."
  :group 'applications)

(defcustom про-новости-отладка nil
  "Включить подробные отладочные сообщения для AI-обработки новостей."
  :type 'boolean
  :group 'про-новости)

(defcustom про-новости-лог-dir
  (expand-file-name "про-новости-лог/" user-emacs-directory)
  "Директория для хранения логов и аналитики новостей."
  :type 'directory)

(defvar-local про-новости--лог-контекст nil
  "Временный контекст (plist) последнего аналитического AI-запроса для логирования.")

;;;; === Макрос-сценарий: открытость и магия повторного использования ===

(defmacro про-новости-сценарий (имя docstring &rest plist)
  "Создаёт интерактивную новостную команду для AI-дайджестов с наборами параметров.
IMЯ — имя функции; DOCSTRING — строка документации.
PLIST — :часов N, :максимум M, :промпт PROMPT, :ссылки T, :фильтр REGEXP.
Любая команда, созданная этот макросом — появляется в общем меню."
  `(defun ,имя ()
     ,docstring
     (interactive)
     (let* ((часов    ,(plist-get plist :часов))
            (максимум ,(plist-get plist :максимум))
            (промпт   ,(or (plist-get plist :промпт) docstring))
            (ссылки   ,(plist-get plist :ссылки))
            (фильтр   ,(plist-get plist :фильтр)))
       (про-новости-выбрать-и-отправить часов фильтр максимум промпт ссылки))))


;;;; === Асинхронный сбор и фильтрация новостей ===

(defun про-новости--получить-асинхронно (секунд on-chunk on-done)
  "Асинхронно получить все новости за СЕКУНД через elfeed, дай on-chunk каждому чанку, по завершении on-done всему списку."
  (if (not (and (boundp 'elfeed-db-entries)
                (hash-table-p elfeed-db-entries)))
      (progn
        (message "База новостей Elfeed недоступна или не загружена! Сначала вызовите M-x elfeed.")
        (funcall on-done nil))
    (let ((новости (cl-remove-if-not
                    (lambda (e)
                      (< (- (float-time) (elfeed-entry-date e)) секунд))
                    (hash-table-values elfeed-db-entries))))
      (message "Собрано %d новостей за указанный период." (length новости))
      (let ((not-entries (cl-remove-if #'elfeed-entry-p новости)))
        (when not-entries
          (message "ВНИМАНИЕ: Найдены элементы не типа elfeed-entry: %S" not-entries)))
      (setq новости (sort новости (lambda (a b) (> (elfeed-entry-date a) (elfeed-entry-date b)))))
      (when (null новости)
        (setq новости (sort (hash-table-values elfeed-db-entries)
                            (lambda (a b) (> (elfeed-entry-date a) (elfeed-entry-date b)))))
        (message "За период новостей нет. Взял все доступные: %d штук." (length новости)))
      (if (null новости)
          (progn
            (message "В базе Elfeed нет новостей вообще! Обновите фиды: M-x elfeed-update")
            (funcall on-done nil))
        (run-at-time 0 nil
                     (lambda ()
                       (let ((result (if on-chunk
                                         (progn
                                           (let ((оригинал (copy-sequence новости))) ;; Сохраняем копию для возврата
                                             (while новости
                                               (funcall on-chunk (cl-subseq новости 0 (min 20 (length новости))))
                                               (setq новости (nthcdr 20 новости)))
                                             оригинал))
                                       новости)))
                         (message "Передано в on-done: %d новостей." (length result))
                         (funcall on-done result))))))))

(defun про-новости--фильтровать (lst regexp &optional exclude-list)
  "Отфильтровать LST по REGEXP (или все), удалить все, где есть EXCLUDE-LIST."
  (let* ((exclude (or exclude-list '("лайфхак" "спецпредложение" "скидки" "Clickbait")))
         (result (cl-remove-if
                  (lambda (x)
                    (or (and regexp (not (string-match-p regexp (elfeed-entry-title x))))
                        (cl-some (lambda (s) (string-match-p (regexp-quote s) (elfeed-entry-title x))) exclude)))
                  lst)))
    (if (null result)
        (progn
          (message "После фильтра с исключениями (%s) ничего не осталось. Пробую без исключений." (string-join exclude ", "))
          (cl-remove-if
           (lambda (x)
             (and regexp (not (string-match-p regexp (elfeed-entry-title x)))))
           lst))
      result)))

(defun про-новости--пакет (новости максимум)
  "Обрезать НОВОСТИ до МАКСИМУМ — только первые MAX."
  (seq-take новости (or максимум 30)))

(defun про-новости--формат (lst &optional links)
  "Формат списка новостей для AI или пользователя."
  (mapconcat (lambda (x)
               (if links (format "- %s (%s)" (car x) (cdr x))
                 (format "- %s" (car x))))
             lst "\n"))

;;;; === Отправка новостей в gptel (AI) ===
(defun про-новости-отправить-via-gptel (итог промпт links)
  (unless (and (listp итог)
               (stringp промпт))
    (error "[про-новости] НЕВЕРНЫЕ аргументы в про-новости-отправить-via-gptel: итог=%S промпт=%S" итог промпт))
  (message "[про-новости] Отправка данных в AI Tunnel...")
  (let* ((строка-списка
          (mapconcat (lambda (entry)
                       (if (consp entry)
                           (format "- %s (%s)" (car entry) (cdr entry))
                         (format "- %s" (format "%s" entry))))
                     итог
                     "\n"))
         (полный-промпт
          (if (and промпт (not (string-empty-p промпт)))
              (concat промпт "\n\n" строка-списка)
            строка-списка)))
    (message "[про-новости] Запрос в AI: %S" полный-промпт)
    (let ((buf (get-buffer-create "*AI News Answer*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert "#+TITLE: AI News Summary\n\n* Reasoning:\n<<waiting>>\n\n* Response:\nЖдите, запрос обрабатывается (streaming)...\n\n"))
      (pop-to-buffer buf)
      (message "[про-новости] Окно с ответом AI открыто, ждите стриминга.")
      (gptel-request
          строка-списка  ;; Позиционный аргумент: основной пользовательский промпт (список новостей)
        :system промпт  ;; Системная инструкция для AI
        :stream t  ;; Включаем стриминг, если поддерживается
        :callback (lambda (response info)  ;; Callback с response и info
                    (if (not response)
                        (let ((err (plist-get info :status)))
                          (message "[про-новости][ОШИБКА] AI Tunnel: %S" err))
                      (when (and (boundp 'про-новости-отладка) про-новости-отладка)
                        (message "[про-новости] Чанк ответа получен от AI Tunnel."))
                      (with-current-buffer buf
                        (save-excursion
                          (let ((content (pcase response
                                           ((pred stringp) response)
                                           (`(reasoning . ,val)
                                            (when (stringp val)
                                              (save-excursion
                                                (goto-char (point-min))
                                                (when (search-forward "* Reasoning:" nil t)
                                                  (forward-line 1)
                                                  (delete-region (point) (line-end-position))
                                                  (insert val)))
                                              nil))  ;; Не вставляем в основной response
                                           (`(content . ,val) (if (stringp val) val (format "%S" val)))
                                           ((pred listp)
                                            (or (alist-get 'content response)
                                                (let ((reas (alist-get 'reasoning response)))
                                                  (when reas
                                                    (save-excursion
                                                      (goto-char (point-min))
                                                      (when (search-forward "* Reasoning:" nil t)
                                                        (forward-line 1)
                                                        (delete-region (point) (line-end-position))
                                                        (insert (if (stringp reas) reas (format "%S" reas)))))
                                                    nil))
                                                (format "%S" response)))
                                           (_ (format "%S" response)))))
                            (when (stringp content)
                              (goto-char (point-max))
                              (insert content)))))))))))

;;;; === Логирование и история аналитики (org) ===

(defun про-новости/логировать (заголовки промпт ответ meta)
  "Логировать анализ в org-файл истории (ЗАГОЛОВКИ, ПРОМПТ, ОТВЕТ, META)."
  (let* ((scenario-name (or (car заголовки) "news"))
         (date (format-time-string "%Y-%m-%d"))
         (log-dir (file-name-as-directory про-новости-лог-dir))
         (file (expand-file-name (format "%s-%s.org" scenario-name date) log-dir)))
    (unless (file-directory-p log-dir)
      (make-directory log-dir t))
    (with-temp-buffer
      (insert (format "#+TITLE: Аналитика %s (%s)\n\n" scenario-name date))
      (insert "* Заголовки:\n" (mapconcat #'car заголовки "\n") "\n\n")
      (when промпт
        (insert "* AI-Инструкция:\n" промпт "\n\n"))
      (when meta (insert "* META:\n" (format "%S" meta) "\n\n"))
      (insert "* Ответ AI:\n" ответ "\n")
      (write-file file))
    file))

(defun про-новости-история/лог-файлы (&optional только-сцен)
  "Список логов истории, можно фильтровать сценарием."
  (let* ((dir (file-name-as-directory про-новости-лог-dir))
         (files (when (file-directory-p dir)
                  (directory-files dir t "\\.org\\'"))))
    (cl-loop for f in files
             when (string-match "\\([^/]+\\)-\\([0-9][0-9][0-9][0-9]-[0-9]+-[0-9]+\\)\\.org\\'" f)
             for scen = (match-string 1 f)
             for date = (match-string 2 f)
             if (or (not только-сцен) (string= только-сцен scen))
             collect (cons (format "%s | %s" scen date) f))))

(defun про-новости-история/выбрать ()
  "Открыть логи истории аналитики."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (choice (completing-read "Логи аналитики: " (mapcar #'car lst) nil t)))
    (when-let ((file (cdr (assoc choice lst))))
      (find-file file)
      (message "Открыт: %s" (abbreviate-file-name file)))))

(defun про-новости-история/последние (n)
  "Открыть последний N логов аналитики."
  (interactive "nСколько последних логов показать: ")
  (let* ((lst (sort (про-новости-история/лог-файлы)
                    (lambda (a b) (string> (cdr a) (cdr b)))))
         (sel (cl-subseq lst 0 (min n (length lst)))))
    (dolist (x sel)
      (find-file-other-window (cdr x))
      (message "Открыт: %s" (car x)))))

(defun про-новости-история/diff ()
  "Сравнить два лога аналитики (визуальный diff)."
  (interactive)
  (let* ((lst (про-новости-история/лог-файлы))
         (lst-names (mapcar #'car lst))
         (a (cdr (assoc (completing-read "Первый лог (старый): " lst-names nil t) lst)))
         (b (cdr (assoc (completing-read "Второй лог (свежий): " lst-names nil t) lst))))
    (when (and a b (file-exists-p a) (file-exists-p b))
      (ediff-files a b))))

;;;; === “Ассистент внимательности”: поиск новых тем ===

(defun про-новости--log-get-words (log-file)
  "Извлечь список уникальных слов из LOG-FILE для мета-анализа."
  (when (and log-file (file-readable-p log-file))
    (with-temp-buffer
      (insert-file-contents log-file)
      (let* ((txt (buffer-string))
             (words (split-string
                     (downcase (replace-regexp-in-string "[^a-zа-яё0-9_-]+" " " txt))
                     " +" t)))
        (delete-dups (cl-remove-if (lambda (w) (< (length w) 4)) words))))))

(defun про-новости--log-recent-files (n &optional сценарий)
  "Последние N лог-файлов, по сценарию если указан."
  (let* ((lst (про-новости-история/лог-файлы сценарий))
         (sorted (sort lst (lambda (a b) (string> (cdr a) (cdr b))))))
    (mapcar #'cdr (cl-subseq sorted 0 (min n (length sorted))))))

(defun про-новости-ассистент/проанализировать-новое (&optional current-log n-compare)
  "Сравнить CURRENT-LOG (или новый) с N-COMPARE предыдущими: найти новые темы."
  (let* ((cur-log (or current-log (car (про-новости--log-recent-files 1))))
         (prev-logs (cdr (про-новости--log-recent-files (or n-compare 8))))
         (cur-words (про-новости--log-get-words cur-log))
         (prev-words (cl-remove-duplicates
                      (apply #'append (mapcar #'про-новости--log-get-words prev-logs))))
         (stop '( "https" "http" "вест" "новост" "замеч" "росси" "главн" "дата"
                  "интернет" "анализ" "ответ" "лог" "дайджест" "инфо" "тема"))
         (new-words (cl-set-difference cur-words prev-words :test #'equal)))
    (cl-remove-if (lambda (w)
                    (or (member w stop)
                        (string-match-p "^[0-9]+$" w)
                        (< (length w) 4)))
                  new-words)))

(defun про-новости-ассистент/инфо-месседж (&optional cur-log n-compare)
  "Мгновенно сообщает, если появились новые темы."
  (let* ((новые (про-новости-ассистент/проанализировать-новое cur-log n-compare)))
    (when (and новые (> (length новые) 0))
      (message "‼️ Ассистент внимательности: Обнаружены новые темы! → %s"
               (string-join (seq-take новые 6) ", "))
      новые)))

(defun про-новости-ассистент/ai-отчет (&optional cur-log n-compare)
  "Генерирует AI-отчёт — что появилось нового, каков тренд."
  (interactive)
  (let* ((recent (про-новости--log-recent-files (or n-compare 8)))
         (cur-log (or cur-log (car recent)))
         (prev-logs (cdr recent)))
    (if (not cur-log)
        (message "Нет ни одного лога истории для AI-отчёта.")
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

;;; --- Интеграция с gptel post-response ---

(defun про-новости--after-ai (beg end)
  "Логирует ответ после AI и запускает ассистента внимательности."
  (let ((response (buffer-substring-no-properties beg end)))
    (when (and (boundp 'про-новости--лог-контекст)
               про-новости--лог-контекст)
      (let* ((params про-новости--лог-контекст)
             (заголовки (plist-get params :заголовки))
             (промпт (plist-get params :промпт))
             (meta (plist-get params :meta)))
        (let ((log-file (про-новости/логировать заголовки промпт response meta)))
          (про-новости-ассистент/инфо-месседж log-file 7)
          (setq-local про-новости--лог-контекст nil))))))

(add-hook 'gptel-post-response-functions #'про-новости--after-ai)

;;;; === Меню: все сценарии и утилиты (с иконками) ===

(defun про-новости/все-сценарии-menu-list ()
  "Собирает меню команд из всех интерактивных сценариев и утилит (с иконками)."
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
            ("про-новости-история/выбрать"   . "📜")
            ("про-новости-история/последние" . "📜")
            ("про-новости-история/diff"      . "📊")
            )))
    (let ((cmds '()))
      (mapatoms
       (lambda (sym)
         (when (and (commandp sym)
                    (string-prefix-p "про-новости-" (symbol-name sym)))
           (let* ((name (symbol-name sym))
                  (icon (or (cdr (assoc name main-icons)) "📰"))
                  (doc (or (documentation sym t) "нет описания"))
                  (lbl (format "%s %s — %s" icon
                               (replace-regexp-in-string
                                "-" " "
                                (string-remove-prefix "про-новости-" name))
                               (replace-regexp-in-string "\n.*" "" doc))))
             (push (cons lbl sym) cmds))))
       obarray)
      ;; Доп. утилиты, фиксированные пункты меню
      (dolist (it '(("📜 История аналитики: выбрать запись"    . про-новости-история/выбрать)
                    ("📜 Последние N логов (откроет окна)"    . про-новости-история/последние)
                    ("📊 Сравнить два среза (diff)"           . про-новости-история/diff)
                    ("🚦 Ассистент внимательности"            . про-новости/меню-ассистент)))
        (push it cmds))
      (sort cmds (lambda (a b) (string< (car a) (car b)))))))

(defun про-новости/меню ()
  "Главное меню: быстро запустите сценарий или утилиту про-новости."
  (interactive)
  (let* ((cmds (про-новости/все-сценарии-menu-list))
         (labels (mapcar #'car cmds))
         (choice (completing-read "Про-Новости: выберите сценарий → " labels nil t))
         (fun    (cdr (assoc choice cmds))))
    (if fun
        (call-interactively fun)
      (message "⏸️ Это не команда."))))

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


;; === Фоновое обновление новостей Elfeed (авто-таймер) ===

(defcustom про-новости-elfeed-автообновление-интервал 1200
  "Частота в секундах (по умолчанию 1200 = 20 минут) обновления фидов Elfeed в фоне."
  :type 'integer
  :group 'про-новости)

(defun про-новости-elfeed-обновлять-в-фоне ()
  "Периодически обновлять Elfeed в фоне, не блокируя основной процесс даже при ошибках."
  (run-at-time "3 min" про-новости-elfeed-автообновление-интервал
               (lambda ()
                 (condition-case err
                     (when (and (featurep 'elfeed)
                                (boundp 'elfeed-db)
                                elfeed-db)
                       (let* ((prev-count (length (or elfeed-db-entries '())))
                              (inhibit-message t))
                         (elfeed-update)
                         ;; Запланировать сообщение через 10 сек (когда обновление завершается)
                         (run-at-time
                          "10 sec" nil
                          (lambda (prev)
                            (let ((new-count (length (or elfeed-db-entries '()))))
                              (when (/= new-count prev)
                                (message "Elfeed: добавлено %d новых новостей."
                                         (- new-count prev)))))
                          prev-count)))
                   (error
                    (message "Elfeed автообновление: %s" (error-message-string err)))))))

;; Запустить авто-таймер при загрузке модуля про-новости:
(про-новости-elfeed-обновлять-в-фоне)

;;;; === Универсальный сценарий по выбору ===

(defun про-новости-выбрать-и-отправить (часов regexp максимум промпт links)
  "Асинхронно собрать дайджест новостей и отправить в gptel.
 - ЧАСОВ назад, REGEXP (строка или все), МАКСИМУМ, ПРОМПТ, LINKS."
  (interactive
   (list (read-number "Часов назад: " 24)
         (read-string  "Фильтр (regexp, пусто = все): " "")
         (read-number  "Максимум новостей: " 30)
         (read-string  "Промпт для AI (Enter — стандарт): ")
         (y-or-n-p     "Включить ссылки? ")))
  (let* ((секунд (* 3600 часов))
         (ai-промпт (unless (string-empty-p промпт) промпт)))
    (про-новости--получить-асинхронно
     секунд
     (lambda (chunk)
       ;; ВАЖНО: фильтруем по elfeed-entry-объектам, не по cons cells!
       (про-новости--фильтровать
        chunk
        (unless (string-empty-p regexp)
          regexp)))
     (lambda (новости)
       (let ((итог (про-новости--пакет
                    (mapcar (lambda (entry)
                              (cons (elfeed-entry-title entry)
                                    (elfeed-entry-link entry)))
                            новости)
                    максимум)))
         (if (null итог)
             (message "Нет подходящих новостей.")
           (про-новости-отправить-via-gptel итог ai-промпт links)))))))

;;;; === Готовые сценарии (добавьте свои через макрос) ===

(про-новости-сценарий
 про-новости/диалектический-анализ
 "Диалектический AI-анализ: выявить противоречия в новостях, описать их, найти синтез."
 :часов 24
 :максимум 20
 :промпт "Проанализируй список новостей в духе диалектики: найди основные противоречия, опиши их в терминах тезиса-антитезиса, предложи возможный синтез и спрогнозируй развитие событий. Форматируй отчёт как краткий аналитический блок для эксперта."
 :ссылки t)

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
 "Айти/наука/технологии — гик-дайджест за 24ч."
 :часов 24 :максимум 40 :ссылки t
 :промпт "Выбери только научные, IT, технологии или Linux/AI новости. Если есть значимые анонсы/софт — выдели отдельно."
 :фильтр "ИИ\\|AI\\|технол\\|наук\\|разработк\\|Linux\\|алгоритм\\|гаджет")

(про-новости-сценарий
 про-новости-кратко
 "AI-компакт: топ-5 событий дня — сразу структурировано."
 :часов 24 :максимум 30
 :промпт "Кратко и по структуре: выбери 4-6 главных события дня, на каждое дай 1-2 предложения пояснения. Итоговый вывод — тренд дня.")

(про-новости-сценарий
 про-новости-темы-радара
 "Инфорадар: ЧТО чаще всего встречается в RSS за 24 часа."
 :часов 24 :максимум 60
 :промпт "Выдели все повторяющиеся или часто встречающиеся темы, упорядочи их по частоте. Покажи по каждой теме — коротко суть, кто участвует, почему это важно.")

(про-новости-сценарий
 про-новости-вопросы-ответы
 "Q&A по лентам суток."
 :часов 24 :максимум 30
 :промпт "Для каждой новости сформулируй адекватный вопрос (что, кто, почему). Сразу дай краткий ответ, основываясь на заголовке и тренде. Кратко и по делу.")

(про-новости-сценарий
 про-новости-юмор
 "Попробуй обернуть события дня в ироничный или юмористический дайджест."
 :часов 24 :максимум 30
 :промпт "Составь юмористический дайджест: перескажи новости дня с лёгкой иронией, подбрось собственный смешной вывод или мем, но не теряй сути событий.")

(про-новости-сценарий
 про-новости-глубоко
 "AI-аналитика: дай неочевидные связи и прогнозы на основе последних 2 суток."
 :часов 48 :максимум 60
 :промпт "Проанализируй полученные новости: найди скрытые тренды, повторяющиеся темы, неочевидные зависимости и взаимосвязи между событиями. Кратко спрогнозируй как это может повлиять на развитие тем в ближайшие дни/недели.")


(provide 'про-новости)
;;; про-новости.el ends here
