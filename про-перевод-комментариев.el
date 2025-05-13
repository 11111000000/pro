;;; про-перевод-комментариев.el --- GPTel overlay translation for comments -*- lexical-binding: t; -*-
;;; Commentary:
;;  Мод переводит все комментарии в буфере с кодом с любого на любой язык 
;;  (по-умолчанию — с английского на русский). Перевод осуществляется через gptel,
;;  используя дефолтную модель с соответствующим промптом. Все комментарии переводятся
;;  одним запросом к нейросети. Сначала собирается массив комментариев, для каждого из
;;  них создаётся отдельный оверлей, который отображает оригинальный текст на контрастном
;;  фоне. После получения ответа от нейросети оверлеи обновляются переведённым текстом и
;;  вставляются в соответствующие места буфера.
;;
;;  При наведении курсора на комментарий оверлей временно скрывается, показывая оригинальный
;;  текст целиком. Если комментарий редактируется, то через минуту без изменений автоматически
;;  отправляется отдельный запрос для перевода изменённого комментария (debounce).
;;
;;  Комментарии определяются универсально для любого поддерживаемого языка. В Emacs Lisp за комментарии
;;  считаются всё, что находится после знака «;» (с объединением последовательных строк) и докстринги функций.
;;
;;  В режиме отладки (включён по-умолчанию) запросы и ответы логируются в буфере *cct-debug*,
;;  который открывается в боковом окне.
;;
;;  Установка:
;;    (require 'про-перевод-комментариев)
;;    (cct-mode 1)
;;
;;  Использование:
;;    M-x cct-translate-buffer
;;
;;  Автор: Ваше Имя

(require 'gptel)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

;; ---------------------------------------------------------------------------
;; Fix for gptel--with-buffer-copy macro
;; ---------------------------------------------------------------------------
;; Исправление ошибки "save-current-buffer: Wrong type argument: char-or-string-p, nil"
;; Переопределяем макрос gptel--with-buffer-copy, чтобы при отсутствии буфера использовался
;; текущий буфер (current-buffer), а при отсутствии имени файла для копии использовалось имя буфера.
(with-eval-after-load 'gptel
  (when (fboundp 'gptel--with-buffer-copy)
    (fmakunbound 'gptel--with-buffer-copy))
  (defmacro gptel--with-buffer-copy (buffer file coding &rest body)
    "Fixed version of gptel--with-buffer-copy to avoid nil buffer or file issues.
If BUFFER is nil, default to `current-buffer'.  If FILE is nil, default to the buffer's name."
    `(with-current-buffer (or ,buffer (current-buffer))
       (let ((file (or ,file (buffer-name))))
         ,@body))))

(defgroup cct nil
  "Translate code comments with GPTel."
  :group 'tools)

(defcustom cct-source-language "English"
  "Source language for translation."
  :type 'string)

(defcustom cct-target-language "Russian"
  "Target language for translation."
  :type 'string)

(defcustom cct-edit-debounce 60
  "Seconds after last edit to re-translate a comment."
  :type 'integer)

(defcustom cct-debug t
  "When non-nil, log all GPT prompts and answers to the *cct-debug* buffer and display it."
  :type 'boolean)

(defvar-local cct--comment-table nil   ;; vector storing comment range info: (start end overlay)
  "Vector storing comment data for each detected comment.")

(defvar-local cct--hovered-ov nil)     ;; currently hovered overlay

;; ---------------------------------------------------------------------------
;; Debug / Logging
;; ---------------------------------------------------------------------------

(defconst cct--log-buffer-name "*cct-debug*"
  "Name of the debug log buffer.")

(defun cct--open-debug-buffer ()
  "Open and display the debug log buffer in a side window."
  (when cct-debug
    (let ((buf (get-buffer-create cct--log-buffer-name)))
      (unless (get-buffer-window buf)
        (display-buffer
         buf
         '((display-buffer-in-side-window)
           (side . right)
           (window-width . 0.3)))))))

(defun cct--log (fmt &rest args)
  "Append a formatted message to the debug log buffer if `cct-debug' is non-nil.
FMT and ARGS are passed to `format'. A timestamp is prepended automatically."
  (when cct-debug
    (with-current-buffer (get-buffer-create cct--log-buffer-name)
      (goto-char (point-max))
      (insert (format-time-string "[%F %T] ")
              (apply #'format fmt args)
              (unless (string-suffix-p "\n" fmt) "\n")))
    (cct--open-debug-buffer)))

;; ---------------------------------------------------------------------------
;; GPTel Request Wrapper with improved logging and error reporting
;; ---------------------------------------------------------------------------
(cl-defun cct--request (prompt &rest args &key callback &allow-other-keys)
  "Wrapper around `gptel-request' that logs PROMPT and the resulting ANSWER.
ARGS are forwarded to `gptel-request'. CALLBACK, if provided, is wrapped so that
responses are logged. В случае ошибки или отсутствия ответа, выводится лог с информацией из INFO.
Также, выводятся явные сообщения, если запрос отправлен или произошла ошибка от gptel."
  (cct--log "Отправляем запрос к GPTel:\n%s\n" prompt)
  (let* ((wrapped-callback
          (lambda (answer info)
            (if (or (null answer) (and (stringp answer) (string-empty-p answer)))
                (progn
                  (cct--log "Ошибка: Пустой ответ от нейросети. Статус: %s, данные: %s"
                            (plist-get info :status) info)
                  (message "Ошибка перевода: пустой ответ, статус: %s" (plist-get info :status)))
              (cct--log "Получен ответ от GPTel:\n%s\n" answer)
              (message "Ответ от нейросети получен."))
            (when callback
              (funcall callback answer info))))
         ;; Ensure we copy and modify the args plist so our :callback and :buffer are set.
         (args-modified (copy-sequence args)))
    (setq args-modified (plist-put args-modified :callback wrapped-callback))
    (setq args-modified (plist-put args-modified :buffer (or (plist-get args-modified :buffer)
                                                             (current-buffer))))
    (prog1
        (apply #'gptel-request prompt args-modified)
      (cct--log "Запрос отправлен, ожидаем ответ..."))))

;; ---------------------------------------------------------------------------
;; Comment detection
;; ---------------------------------------------------------------------------
(defun cct--ranges-with-comment-start ()
  "Return a list of (START . END) pairs for comments using `comment-start'.
Используется, когда переменная `comment-start' определена для языка."
  (when (and comment-start (not (string= comment-start "")))
    (save-excursion
      (goto-char (point-min))
      (let (acc cs ce)
        (setq cs (regexp-quote comment-start))
        (setq ce (and comment-end (not (string= comment-end "")) (regexp-quote comment-end)))
        (while (search-forward-regexp cs nil t)
          (let* ((beg (match-beginning 0))
                 (end (if ce
                          (if (search-forward-regexp ce nil t)
                              (point)
                            (line-end-position))
                        (line-end-position))))
            (push (cons beg end) acc)))
        (nreverse acc)))))

(defun cct--ranges-with-syntax ()
  "Collect comment ranges using syntax parsing via `syntax-ppss'.
Используется как запасной метод, если `comment-start' не задан."
  (save-excursion
    (goto-char (point-min))
    (let (acc)
      (while (not (eobp))
        (let ((state (syntax-ppss)))
          (if (nth 4 state)
              (let ((beg (nth 8 state)))
                (forward-comment 1)
                (push (cons beg (point)) acc))
            (forward-line 1)))
        (nreverse acc)))))

(defun cct--collect-comment-ranges-elisp ()
  "Собирает комментарии для Emacs Lisp: объединяет строки, начинающиеся с ;,
а также извлекает докстринги функций."
  (save-excursion
    (let (ranges start end line)
      (goto-char (point-min))
      ;; Собираем блоки комментариев, начинающихся с ';'
      (while (not (eobp))
        (setq line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
        (if (string-match-p "^[ \t]*;" line)
            (progn
              (setq start (line-beginning-position))
              (setq end (line-end-position))
              (forward-line 1)
              (while (and (not (eobp))
                          (let ((next-line (buffer-substring-no-properties
                                            (line-beginning-position) (line-end-position))))
                            (string-match-p "^[ \t]*;" next-line)))
                (setq end (line-end-position))
                (forward-line 1))
              (push (cons start end) ranges))
          (forward-line 1)))
      ;; Собираем докстринги функций
      (goto-char (point-min))
      (while (re-search-forward "^(def\\(un\\|macro\\)[ \t]+\\(?:\\sw\\|\\s_\\)+" nil t)
        (let ((defun-start (match-beginning 0)))
          (ignore-errors
            (forward-sexp 2)
            (skip-chars-forward " \t\n")
            (when (and (looking-at "\"")
                       (not (nth 3 (syntax-ppss))))
              (let ((doc-start (point)))
                (condition-case nil
                    (progn
                      (forward-sexp)
                      (push (cons doc-start (point)) ranges))
                  (error nil)))))))
      (nreverse ranges))))

(defun cct--collect-comment-ranges ()
  "Собирает диапазоны комментариев универсальным методом.
Если режим Emacs Lisp, используются правила объединения строк и извлечения докстрингов.
Иначе, применяется либо метод с `comment-start', либо синтаксический анализ."
  (if (derived-mode-p 'emacs-lisp-mode)
      (cct--collect-comment-ranges-elisp)
    (or (cct--ranges-with-comment-start)
        (cct--ranges-with-syntax))))

;; ---------------------------------------------------------------------------
;; Prompt Building
;; ---------------------------------------------------------------------------
(defun cct--build-global-prompt (comment-strings)
  "Построить единый промпт для массива COMMENT-STRINGS (вектор).
Промпт инструктирует AI перевести каждый комментарий с `cct-source-language'
на `cct-target-language'. Ожидается, что AI вернёт JSON-массив объектов вида:
  [{\"id\":0,\"translation\":\"…\"}, ...] без лишнего текста."
  (let ((header (format "You are given an array of code comments. Translate each one from %s to %s. 
Preserve comment delimiters (//, #, /* … */ etc.) and line-breaks.
Respond EXACTLY with a JSON array of objects like
[{\"id\":0,\"translation\":\"…\"}, ...] without any extra text.\n\n"
                        cct-source-language cct-target-language))
        (comments "COMMENTS:\n")
        (len (length comment-strings)))
    (dotimes (i len)
      (setq comments (concat comments (format "%d: %s\n" i (aref comment-strings i)))))
    (concat header comments)))

;; ---------------------------------------------------------------------------
;; Overlay Helpers
;; ---------------------------------------------------------------------------
(defun cct--create-overlay (start end &optional translation waiting)
  "Создать или обновить оверлей между START и END.
Если TRANSLATION задан, оверлей отображает переведённый текст с `cct-translation-face'.
Если WAITING не nil, отображается оригинальный текст с `cct-waiting-face'."
  (let* ((existing (get-text-property start 'cct-ov))
         (ov (or existing (make-overlay start end nil t t))))
    (overlay-put ov 'cct t)
    (if translation
        (progn
          (overlay-put ov 'cct-translation translation)
          (overlay-put ov 'display translation)
          (overlay-put ov 'face 'cct-translation-face))
      (overlay-put ov 'cct-translation nil)
      (overlay-put ov 'display (buffer-substring-no-properties start end))
      (overlay-put ov 'face 'cct-waiting-face))
    (overlay-put ov 'help-echo "Original comment (hover)")
    (put-text-property start end 'cct-ov ov)
    ov))

(defun cct--hide-or-show (ov show-original)
  "Скрыть или показать оверлей OV.
Если SHOW-ORIGINAL не nil, скрываем перевод, показывая оригинальный текст,
иначе, если перевод уже получен, отображаем его."
  (if show-original
      (overlay-put ov 'display (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
    (when-let ((tr (overlay-get ov 'cct-translation)))
      (overlay-put ov 'display tr))))

(defun cct--post-command-hide/show ()
  "Хук, переключающий отображение оверлея в зависимости от положения курсора.
Если курсор находится над комментарием, временно скрывается перевод и показывается оригинал.
При уходе курсора отображается переведённый текст, если он уже загружен."
  (let* ((ovs (overlays-at (point)))
         (ov (seq-find (lambda (o) (overlay-get o 'cct)) ovs)))
    (unless (eq ov cct--hovered-ov)
      (when (and (overlayp cct--hovered-ov)
                 (overlay-get cct--hovered-ov 'cct))
        (cct--hide-or-show cct--hovered-ov nil))
      (setq cct--hovered-ov ov)
      (when (overlayp ov)
        (cct--hide-or-show ov t)))))

;; ---------------------------------------------------------------------------
;; Re-translation on Edit (Debounce)
;; ---------------------------------------------------------------------------
(defun cct--schedule-retranslate (ov)
  "Планировать повторный перевод комментария, соответствующего оверлею OV, через `cct-edit-debounce' секунд без изменений."
  (when-let ((tm (overlay-get ov 'cct-timer)))
    (cancel-timer tm))
  (overlay-put ov 'cct-timer
               (run-with-idle-timer cct-edit-debounce nil #'cct--retranslate-overlay ov)))

(defun cct--retranslate-overlay (ov)
  "Отправить отдельный запрос на перевод изменённого комментария, соответствующего оверлею OV.
Выводит сообщения об успешном обновлении или ошибке от gptel."
  (let* ((txt (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
         (prompt (cct--build-global-prompt (vector txt))))
    (cct--request prompt
                  :callback
                  (lambda (answer _info)
                    (if (or (null answer) (and (stringp answer) (string-empty-p answer)))
                        (progn
                          (cct--log "Ошибка: нет ответа нейросети при обновлении перевода для оверлея на позиции %d"
                                    (overlay-start ov))
                          (message "Ошибка обновления перевода для комментария на позиции %d" (overlay-start ov)))
                      (condition-case err
                          (let* ((json-array (json-parse-string answer))
                                 (obj (aref json-array 0))
                                 (tr (gethash "translation" obj)))
                            (if tr
                                (progn
                                  (overlay-put ov 'cct-translation tr)
                                  (overlay-put ov 'display tr)
                                  (overlay-put ov 'face 'cct-translation-face)
                                  (cct--log "Updated translation for edited comment at %d" (overlay-start ov))
                                  (message "Обновлён перевод для комментария на позиции %d" (overlay-start ov)))
                              (cct--log "Ошибка: отсутствует ключ 'translation' в ответе для оверлея на позиции %d"
                                        (overlay-start ov))))
                        (error (cct--log "Error re-translating comment at %d: %s"
                                         (overlay-start ov) err))))))))

(defun cct--after-change (beg _end _len)
  "При изменениях текста у позиции BEG планирует повторный перевод комментария, если там есть оверлей."
  (dolist (ov (overlays-at beg))
    (when (overlay-get ov 'cct)
      (cct--schedule-retranslate ov))))

;; ---------------------------------------------------------------------------
;; Main Entry Point: Перевод всех комментариев одним запросом
;; ---------------------------------------------------------------------------
;;;###autoload
(defun cct-translate-buffer (&optional ask-langs)
  "Перевести все комментарии в текущем буфере одним запросом к GPT.
Если ASK-LANGS не nil, предложить ввести исходный и целевой языки для перевода.
Для каждого найденного комментария создаётся оверлей с оригинальным текстом на контрастном фоне.
После получения ответа от нейросети, оверлеи обновляются переведённым текстом.
При ошибке запроса выводится сообщение с информацией от gptel."
  (interactive "P")
  (when ask-langs
    (setq cct-source-language (read-string "Source language: " cct-source-language)
          cct-target-language (read-string "Target language: " cct-target-language)))
  (when cct-debug (cct--open-debug-buffer))
  (let* ((ranges (cct--collect-comment-ranges))
         (count  (length ranges))
         (texts  (make-vector count nil)))
    (if (zerop count)
        (progn
          (message "Комментариев не найдено в буфере.")
          nil)
      ;; Сохранить оригинальные тексты комментариев и создать таблицу оверлеев.
      (setq cct--comment-table (make-vector count nil))
      (cl-loop for (beg . end) in ranges
               for i from 0
               do (let ((txt (string-trim (buffer-substring-no-properties beg end)))
                        (ov (cct--create-overlay beg end nil t)))
                    (aset texts i txt)
                    (aset cct--comment-table i (list beg end ov))))
      (cct--log "Collected comments:\n%s" (prin1-to-string (append texts nil)))
      ;; Отправить единый запрос на перевод всех комментариев.
      (cct--request (cct--build-global-prompt texts)
                    :callback
                    (lambda (answer _info)
                      (if (or (null answer) (and (stringp answer) (string-empty-p answer)))
                          (cct--log "Ошибка: Пустой ответ от нейросети при переводе буфера.")
                        (condition-case err
                            (let ((json-array (json-parse-string answer)))
                              (dotimes (i (length json-array))
                                (let* ((obj   (aref json-array i))
                                       (idx   (gethash "id" obj))
                                       (tr    (gethash "translation" obj))
                                       (cell  (aref cct--comment-table idx))
                                       (ov    (nth 2 cell)))
                                  (if (and cell tr ov)
                                      (progn
                                        (overlay-put ov 'cct-translation tr)
                                        (overlay-put ov 'display tr)
                                        (overlay-put ov 'face 'cct-translation-face)
                                        (cct--log "Updated translation for comment id=%d (pos %d)" idx (nth 0 cell)))
                                    (cct--log "Ошибка: Недостаточно данных для обновления перевода комментария id=%s" idx))))
                              (error (cct--log "Error parsing AI response in buffer translation: %s" err)))
                          (message "Перевод комментариев завершён."))))))))

;; ---------------------------------------------------------------------------
;; Minor Mode Definition
;; ---------------------------------------------------------------------------
;;;###autoload
(define-minor-mode cct-mode
  "Малый режим для автоматического перевода комментариев с использованием оверлеев.
При включении все комментарии переводятся через GPTel, и поверх оригинального текста
устанавливаются оверлеи с переводом. Пока перевод не получен, отображается оригинальный
текст с контрастным фоном. При наведении курсора на комментарий оверлей временно скрывается,
а редактирование вызывает повторный перевод после задержки (debounce)."
  :lighter " CCT"
  (if cct-mode
      (progn
        (add-hook 'post-command-hook   #'cct--post-command-hide/show nil t)
        (add-hook 'after-change-functions #'cct--after-change      nil t)
        (cct--open-debug-buffer)
        (message "cct-mode включён. Для перевода комментариев используйте M-x cct-translate-buffer."))
    ;; Отключение режима: удалить хуки и все созданные оверлеи.
    (remove-hook 'post-command-hook   #'cct--post-command-hide/show t)
    (remove-hook 'after-change-functions #'cct--after-change      t)
    (mapc #'delete-overlay
          (seq-filter (lambda (ov) (overlay-get ov 'cct))
                      (overlays-in (point-min) (point-max))))
    (setq cct--hovered-ov nil
          cct--comment-table nil)
    (message "cct-mode отключён.")))

(provide 'про-перевод-комментариев)
