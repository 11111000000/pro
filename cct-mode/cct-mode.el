;;; cct-mode.el --- GPTel overlay translation for comments -*- lexical-binding: t; -*-
;;; Commentary:
;;  Мод переводит комментарии в буфере с кодом с любого на любой язык
;;  (по-умолчанию — с английского на русский). Перевод осуществляется через
;;  gptel, используя дефолтную модель с соответствующим промптом.
;;  При создании комментариев, сначала собирается
;;  массив комментариев, для каждого из них вычисляется хэш. После получения ответа от нейросети
;;  создаётся оверлей или обновляется переведённым текстом и вставляется в соответствующие места буфера.
;;  Переводы кэшируются по хэшсумме текста комментария. Кэш сохраняется в специально именованных файлах
;;  и восстанавливается при включении cct-mode, если он существует для этого файла.
;;  Все комментарии переводятся одним запросом к нейросети.
;;  При наведении курсора на оверлей перевод временно скрывается,
;;  показывая оригинальный текст и позволяя его редактировать.

;;  Комментарии определяются универсально для любого поддерживаемого языка.
;;  В Emacs Lisp за комментарии считаются всё, что находится после знака «;»
;;  (с объединением последовательных строк) и докстринги функций.

;;  В режиме отладки (по умолчанию включён) запросы и ответы логируются в
;;  буфере *cct-debug*, который открывается в боковом окне.

;;  Установка:
;;    (require 'cct-mode)
;;    (cct-mode 1)

;;  Использование:
;;    M-x cct-translate-buffer

;;  Автор: Petr

(require 'gptel)
(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'seq)

(defun cct--strip-code-fence (s)
  "Remove Markdown code fence markers from S if present."
  (if (and s (string-match "```json\\s-*\n\\([\\s\\S\n\r]*?\\)\\n```\\s-*\\'" s))
      (match-string 1 s)
    s))

(defun cct--process-response (answer)
  "Process ANSWER string: trim, strip code fence and parse JSON.
If ANSWER is nil or empty, return an empty vector to avoid errors."
  (let ((cleaned (cct--strip-code-fence (string-trim (or answer "")))))
    (condition-case err
        (if (or (null cleaned) (string-empty-p cleaned))
            []  ;; empty vector
          (json-parse-string cleaned))
      (error
       (cct--log "Ошибка разбора JSON: %s" err)
       []))))

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

(defface cct-waiting-face
  '((((class color) (min-colors 88))
     :background "#444444")
    (t :inverse-video t))
  "Face used while waiting for a translation."
  :group 'cct)

(defface cct-translation-face
  '((((class color) (min-colors 88))
     :background "#333333")
    (t :inverse-video t))
  "Face used for translated comments."
  :group 'cct)

(defvar-local cct--comment-table nil
  "Vector storing comment data for each detected comment.
Для каждого комментария хранится список: (начало конец оверлей хэш перевода).
Хэш используется для кэширования перевода и обновления оверлеев.")

(defvar-local cct--hovered-ov nil
  "Currently hovered CCT overlay.")

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
FMT and ARGS are passed to `format'."
  (when cct-debug
    (with-current-buffer (get-buffer-create cct--log-buffer-name)
      (goto-char (point-max))
      (let ((text (apply #'format fmt args)))
        (insert (format-time-string "[%F %T] ")
                text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))))
    (cct--open-debug-buffer)))

(cl-defun cct--request (prompt &rest args &key callback &allow-other-keys)
  "Wrapper around `gptel-request' that logs PROMPT and processes the ANSWER asynchronously.
ARGS are forwarded to `gptel-request'."
  (cct--log "Отправляем запрос к GPTel:\n%s\n" prompt)
  (let* ((wrapped-callback
          (lambda (answer info)
            (if (or (null answer)
                    (and (stringp answer) (string-empty-p answer)))
                (progn
                  (cct--log "Ошибка: пустой ответ от нейросети. Статус: %s\nДанные: %s"
                            (plist-get info :status) info)
                  (message "Ошибка перевода: пустой ответ, статус: %s" (plist-get info :status)))
              (cct--log "Получен ответ от GPTel:\n%s\n" answer)
              (let ((result (condition-case err
                                (cct--process-response answer)
                              (error
                               (cct--log "Ошибка при разборе ответа перевода комментария: %s" err)
                               (message "Ошибка разбора JSON ответа: %s" err)
                               nil))))
                (when result
                  (message "Ответ от нейросети успешно обработан.")))
              (when callback
                (funcall callback answer info)))))
         (args-plist (cl-copy-list args)))
    (setq args-plist (plist-put args-plist :callback wrapped-callback))
    (setq args-plist (plist-put args-plist :buffer (or (plist-get args-plist :buffer)
                                                       (current-buffer))))
    (unless (plist-member args-plist :stream)
      (setq args-plist (plist-put args-plist :stream t)))
    (prog1
        (apply #'gptel-request prompt args-plist)
      (cct--log "Запрос отправлен, ожидаем ответ…"))))

(defun cct--collect-comment-ranges ()
  "Collect comment ranges using syntax parsing via `syntax-ppss'.
Handles line comments and block comments, skips comment markers inside strings.
For Emacs Lisp also collects docstrings."
  (if (derived-mode-p 'emacs-lisp-mode)
      (let (ranges start end)
        (save-excursion
          (goto-char (point-min))
          ;; Line comments
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (when (and (string-match-p "^[ \t]*;" line)
                         (not (nth 3 (syntax-ppss (line-beginning-position)))))
                (setq start (line-beginning-position)
                      end (line-end-position))
                (forward-line 1)
                (while (and (not (eobp))
                            (let ((next (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                              (and (string-match-p "^[ \t]*;" next)
                                   (not (nth 3 (syntax-ppss (line-beginning-position)))))))
                  (setq end (line-end-position))
                  (forward-line 1))
                (push (cons start end) ranges)))
            (forward-line 1))
          ;; Docstrings
          (goto-char (point-min))
          (while (re-search-forward "^(def\\(un\\|macro\\|var\\)[ \t]+\\(?:\\sw\\|\\s_\\)+" nil t)
            (ignore-errors
              (forward-sexp 2)
              (skip-chars-forward " \t\n")
              (when (and (looking-at "\"")
                         (not (nth 3 (syntax-ppss))))
                (let ((doc-start (point)))
                  (forward-sexp)
                  (push (cons doc-start (point)) ranges)))))
          ;; defcustom, defgroup docstrings
          (goto-char (point-min))
          (while (re-search-forward "^(def\\(custom\\|group\\)[ \t]+\\(?:\\sw\\|\\s_\\)+" nil t)
            (ignore-errors
              (forward-sexp 3)
              (skip-chars-forward " \t\n")
              (when (and (looking-at "\"")
                         (not (nth 3 (syntax-ppss))))
                (let ((doc-start (point)))
                  (forward-sexp)
                  (push (cons doc-start (point)) ranges)))))
          (nreverse ranges)))
    (let (acc)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (let ((state (syntax-ppss)))
            (cond
             ((nth 4 state)
              (let ((beg (nth 8 state)))
                (forward-comment 1)
                (push (cons beg (point)) acc)))
             ((nth 3 state)
              (goto-char (nth 8 state))
              (forward-sexp 1))
             (t (forward-line 1)))))
        (nreverse acc)))))

(defun cct--build-global-prompt (comment-strings)
  "Build a single prompt asking to translate COMMENT-STRINGS vector."
  (let ((header (format
                 "You are given an array of code comments. Translate each one from %s to %s.\n\
Preserve comment delimiters (//, #, /* … */ etc.) and line-breaks.\n\
Respond EXACTLY with a JSON array of objects like\n\
[{\"id\":0,\"translation\":\"…\"}, ...] without any extra text.\n\n"
                 cct-source-language cct-target-language))
        (comments "COMMENTS:\n")
        (len (length comment-strings)))
    (dotimes (i len)
      (setq comments (concat comments (format "%d: %s\n" i (aref comment-strings i)))))
    (concat header comments)))

(defun cct--propertized (str face)
  "Return STR propertized with FACE, keeping text properties shallow."
  (propertize str 'face face))

(defun cct--create-overlay (start end &optional translation waiting)
  "Create or update overlay for comment between START and END.
If TRANSLATION is non-nil show it, otherwise show original.
If WAITING is non-nil highlight as waiting for translation."
  (let* ((existing (get-text-property start 'cct-ov))
         (ov (or existing (make-overlay start end nil t t))))
    (overlay-put ov 'cct t)
    (if translation
        (progn
          (overlay-put ov 'cct-translation translation)
          (overlay-put ov 'display (cct--propertized translation 'cct-translation-face)))
      (overlay-put ov 'cct-translation nil)
      (overlay-put ov 'display (cct--propertized
                                (buffer-substring-no-properties start end)
                                'cct-waiting-face)))
    (overlay-put ov 'help-echo "Original comment (hover)")
    (put-text-property start end 'cct-ov ov)
    ov))

(defun cct--hide-or-show (ov show-original)
  "Toggle OV between original text and translation.
If SHOW-ORIGINAL is non-nil, show original text for editing.
Otherwise show translation."
  (if show-original
      (progn
        (overlay-put ov 'display nil)
        (overlay-put ov 'modification-hooks nil)
        (overlay-put ov 'insert-in-front-hooks nil)
        (overlay-put ov 'insert-behind-hooks nil))
    (when-let ((tr (overlay-get ov 'cct-translation)))
      (overlay-put ov 'display (cct--propertized tr 'cct-translation-face))
      (overlay-put ov 'modification-hooks
                   (list (lambda (o &rest _) (cct--hide-or-show o t))))
      (overlay-put ov 'insert-in-front-hooks
                   (list (lambda (o &rest _) (cct--hide-or-show o t))))
      (overlay-put ov 'insert-behind-hooks
                   (list (lambda (o &rest _) (cct--hide-or-show o t)))))))

(defun cct--post-command-hide/show ()
  "Reveal original comment under point, hide translation.
If cursor is on overlay, show original text.
If cursor leaves overlay, show translation."
  (let* ((ovs (overlays-at (point)))
         (ov (seq-find (lambda (o) (overlay-get o 'cct)) ovs)))
    (unless (eq ov cct--hovered-ov)
      (when (and (overlayp cct--hovered-ov)
                 (overlay-get cct--hovered-ov 'cct))
        (cct--hide-or-show cct--hovered-ov nil))
      (setq cct--hovered-ov ov)
      (when (overlayp ov)
        (cct--hide-or-show ov t)))))

(defun cct--schedule-retranslate (ov)
  "Schedule re-translation for OV after `cct-edit-debounce' seconds."
  (when-let ((tm (overlay-get ov 'cct-timer)))
    (cancel-timer tm))
  (overlay-put ov 'cct-timer
               (run-with-idle-timer cct-edit-debounce nil
                                    #'cct--retranslate-overlay ov)))

(defun cct--retranslate-overlay (ov)
  "Send translation request for changed comment OV."
  (let* ((txt (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
         (prompt (cct--build-global-prompt (vector txt))))
    (cct--request
     prompt
     :callback
     (lambda (answer _info)
       (if (or (null answer)
               (and (stringp answer) (string-empty-p answer)))
           (progn
             (cct--log "Ошибка: нет ответа при обновлении перевода (pos %d)" (overlay-start ov))
             (message "Ошибка обновления перевода для комментария (pos %d)" (overlay-start ov)))
         (condition-case err
             (let* ((json-array (cct--process-response answer))
                    (obj (aref json-array 0))
                    (tr (gethash "translation" obj)))
               (if tr
                   (progn
                     (overlay-put ov 'cct-translation tr)
                     (if (eq cct--hovered-ov ov)
                         (overlay-put ov 'display nil)
                       (overlay-put ov 'display (cct--propertized tr 'cct-translation-face)))
                     (cct--log "Updated translation for edited comment at %d" (overlay-start ov))
                     (message "Обновлён перевод комментария (pos %d)" (overlay-start ov)))
                 (cct--log "Ошибка: отсутствует ключ 'translation' (pos %d)" (overlay-start ov))))
           (error
            (cct--log "Error re-translating comment (pos %d): %s" (overlay-start ov) err))))))))

(defun cct--after-change (beg _end _len &rest _)
  "Schedule re-translation when comment near BEG changed."
  (dolist (ov (overlays-at beg))
    (when (overlay-get ov 'cct)
      (cct--schedule-retranslate ov))))

;;;###autoload
(defun cct-translate-buffer (&optional ask-langs)
  "Translate every comment in current buffer in one GPT query.
Сначала собирается массив комментариев, для каждого вычисляется хэш.
После получения ответа создаются или обновляются оверлеи с переводом.
Переводы кэшируются по хэшу, кэш сохраняется и восстанавливается при включении cct-mode."
  (interactive "P")
  (when ask-langs
    (setq cct-source-language (read-string "Source language: " cct-source-language)
          cct-target-language (read-string "Target language: " cct-target-language)))
  (when cct-debug (cct--open-debug-buffer))
  (let* ((ranges (cct--collect-comment-ranges))
         (count (length ranges))
         (texts (make-vector count nil)))
    (if (zerop count)
        (message "Комментариев не найдено в буфере.")
      (setq cct--comment-table (make-vector count nil))
      (cl-loop for (beg . end) in ranges
               for i from 0
               do (let* ((txt (string-trim (buffer-substring-no-properties beg end)))
                         (hash (secure-hash 'sha256 txt))
                         (ov (make-overlay beg end nil t t)))
                    (overlay-put ov 'cct t)
                    (overlay-put ov 'cct-translation nil)
                    (overlay-put ov 'display (cct--propertized (buffer-substring-no-properties beg end) 'cct-waiting-face))
                    (overlay-put ov 'help-echo "Original comment (hover)")
                    (put-text-property beg end 'cct-ov ov)
                    (aset texts i txt)
                    (aset cct--comment-table i (list beg end ov hash nil))))
      (cct--log "Collected comments:\n%s" (prin1-to-string (append texts nil)))
      ;; TODO: загрузка кэша из файлов по хэшу, если реализовано
      (cct--request
       (cct--build-global-prompt texts)
       :callback
       (lambda (answer _info)
         (if (or (null answer)
                 (and (stringp answer) (string-empty-p answer)))
             (cct--log "Ошибка: пустой ответ при переводе буфера.")
           (condition-case err
               (let ((json-array (cct--process-response answer)))
                 (dotimes (i (length json-array))
                   (let* ((obj (aref json-array i))
                          (idx (gethash "id" obj))
                          (tr (gethash "translation" obj))
                          (cell (and idx (aref cct--comment-table idx))))
                     (pcase-let ((`(,beg ,end ,ov ,hash ,old-tr) cell))
                       (when (and beg end ov tr)
                         (overlay-put ov 'cct-translation tr)
                         (overlay-put ov 'display (cct--propertized tr 'cct-translation-face))
                         ;; Обновляем кэш перевода
                         (aset cct--comment-table idx (list beg end ov hash tr))
                         (cct--log "Updated translation id=%d (pos %d)" idx beg)))))
                 (message "Перевод комментариев завершён."))
             (error
              (cct--log "Error parsing AI response: %s" err)
              (message "Ошибка разбора JSON ответа: %s" err)))))))))

;;;###autoload
(define-minor-mode cct-mode
  "Minor mode for automatic translation of code comments using GPTel overlays."
  :lighter " CCT"
  (if cct-mode
      (progn
        (add-hook 'post-command-hook #'cct--post-command-hide/show nil t)
        (add-hook 'after-change-functions #'cct--after-change nil t)
        (cct--open-debug-buffer)
        (message "cct-mode включён. Для перевода используйте M-x cct-translate-buffer."))
    (remove-hook 'post-command-hook #'cct--post-command-hide/show t)
    (remove-hook 'after-change-functions #'cct--after-change t)
    (mapc #'delete-overlay
          (seq-filter (lambda (ov) (overlay-get ov 'cct))
                      (overlays-in (point-min) (point-max))))
    (setq cct--hovered-ov nil
          cct--comment-table nil)
    (message "cct-mode отключён.")))

(defun cct--comment-range-at-point ()
  "Return (START . END) of comment at point, or nil if none.
Try fast detection using `syntax-ppss', fallback to full scan."
  (save-excursion
    (let ((state (syntax-ppss)))
      (cond
       ((nth 4 state)
        (let ((beg (nth 8 state)))
          (goto-char beg)
          (ignore-errors (forward-comment 1))
          (cons beg (point))))
       ((and (derived-mode-p 'emacs-lisp-mode) (nth 3 state))
        (let ((beg (nth 8 state)))
          (goto-char beg)
          (ignore-errors (forward-sexp))
          (cons beg (point))))
       (t
        (let* ((pos (point))
               (local-start (max (point-min) (- pos 1000)))
               (local-end (min (point-max) (+ pos 1000)))
               (ranges (save-restriction
                         (narrow-to-region local-start local-end)
                         (cct--collect-comment-ranges))))
          (or (seq-find (lambda (range)
                          (and (>= pos (car range))
                               (<= pos (cdr range))))
                        ranges)
              (let ((ranges (cct--collect-comment-ranges)))
                (seq-find (lambda (range)
                            (and (>= pos (car range))
                                 (<= pos (cdr range))))
                          ranges)))))))))

;;;###autoload
(defun cct-translate-at-point ()
  "Translate comment at point via GPTel.
Show overlay with original text while waiting.
Show overlay with translation after response."
  (interactive)
  (let ((range (cct--comment-range-at-point)))
    (if (not range)
        (message "Курсор не на комментарии.")
      (let* ((beg (car range))
             (end (cdr range))
             (txt (string-trim (buffer-substring-no-properties beg end)))
             (ov (cct--create-overlay beg end nil t))
             (prompt (cct--build-global-prompt (vector txt))))
        (cct--log "Перевод комментария на месте: %s" txt)
        (run-at-time 0 nil
                     (lambda ()
                       (cct--request
                        prompt
                        :callback
                        (lambda (answer _info)
                          (if (or (null answer)
                                  (and (stringp answer) (string-empty-p answer)))
                              (progn
                                (cct--log "Ошибка: пустой ответ при переводе комментария на месте.")
                                (message "Ошибка перевода комментария."))
                            (condition-case err
                                (let* ((json-array (cct--process-response answer))
                                       (obj (aref json-array 0))
                                       (tr (gethash "translation" obj)))
                                  (if tr
                                      (progn
                                        (overlay-put ov 'cct-translation tr)
                                        (if (eq cct--hovered-ov ov)
                                            (overlay-put ov 'display nil)
                                          (overlay-put ov 'display (cct--propertized tr 'cct-translation-face)))
                                        (cct--log "Перевод комментария на месте обновлён (pos %d)" beg)
                                        (message "Перевод комментария завершён."))
                                    (cct--log "Ошибка: отсутствует ключ 'translation' в ответе.")))
                              (error
                               (cct--log "Ошибка при разборе ответа перевода комментария: %s" err)
                               (message "Ошибка при разборе ответа перевода комментария."))))))))))))

(provide 'cct-mode)
;;; cct-mode.el ends here
