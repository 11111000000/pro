;;; про-интернет-общение.el --- Команды взаимодействия с telega через consult -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 0.2
;; Keywords: telega, consult, chat, contacts
;; URL: (локальный, в составе пользовательской конфигурации)
;;
;;; Commentary:
;;
;; Этот файл объединяет выбор чатов/групп/каналов и контактов telega в одной
;; удобной команде на базе consult.  Предоставляет:
;; - Единый интерактивный список (чаты + пользователи);
;; - Аннотации кандидатов ([chat]/[contact]) и краткие метрики непрочитанных;
;; - Narrowing (c — только чаты, u — только контакты);
;; - Предпросмотр (по умолчанию в echo-area, можно переключить на help-window);
;; - Дедупликацию контактов, у которых есть приватный чат;
;; - Сортировку чатов по активным критериям telega, пользователей — по telega-user>.
;;
;; Файл устроен как «литературный код».  Читайте комментарии, они описывают замыслы,
;; компромиссы и решения.  Настройки доступны в группе ‘pro/интернет’.

;;; Code:

;;;; Telegram: установка и базовая настройка

(require 'установить-из)

(use-package telega
  :ensure t
  :defer t
  :init (установить-из :repo "zevlg/telega.el")
  :custom ((telega-use-docker t)
           (telega-chat-list-default-filter "Unread")
           (telega-use-images t)
           (telega-emoji-use-images nil)
           (telega-emoji-font-family nil)
           (telega-chat-show-avatars nil)
           (telega-chat-show-photos nil)
           (telega-root-auto-fill-mode nil)
           (telega-chat-auto-fill-mode nil)
           (telega-webpage-preview-mode nil)
           (telega-chat-fill-column 80)
           (telega-chat-history-limit 100)
           (telega-emoji-font-family "Noto Color Emoji"))
  :hook ((telega-root-mode . telega-notifications-mode)
         (telega-root-mode . (lambda () (telega-root-auto-fill-mode -1)))
         (telega-chat-mode . (lambda () (telega-chat-auto-fill-mode -1)))
         (telega-load-hook . global-telega-url-shorten-mode)
         (telega-root-mode . hl-line-mode))
  :config)

(defun telega-close-idle-chat-buffers ()
  "Автоматически закрывает неактивные telega-буферы."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*Telega Chat" (buffer-name buf))
      (kill-buffer buf))))

;;;; Telegram: CAPF упоминаний (@username, полное имя)

(defun pro/telega--mention-alist ()
  "Вернуть alist (username . fullname) для контактов.
fullname — склеенные first_name и last_name (может быть nil/пустым).
Если у пользователя несколько активных никнеймов — вернуть все."
  (cl-loop for u in (pro/telega--contacts)
           for fname = (telega--tl-get u :first_name)
           for lname = (telega--tl-get u :last_name)
           for full = (string-trim (mapconcat #'identity
                                              (delq nil (list fname lname))
                                              " "))
           for primary-uname = (telega--tl-get u :username)
           for active = (let ((act (telega--tl-get u :usernames :active_usernames)))
                          (cond
                           ((vectorp act) (append act nil))
                           ((listp act) act)
                           (t nil)))
           for unames = (delete-dups (delq nil (append (list primary-uname) active)))
           append (mapcar (lambda (un) (cons un (unless (string-empty-p full) full)))
                          unames)
           ;; Запись без username — для поиска по имени
           when (not (string-empty-p full))
           collect (cons nil full)))

(defun pro/telega-mention-capf ()
  "CAPF для автодополнения @упоминаний в telega чатах.
Ищет по нику и по имени (включая кириллицу)."
  (when (derived-mode-p 'telega-chat-mode)
    (let* ((pt (point))
           (at-pos (save-excursion
                     (when (search-backward "@" (line-beginning-position) t)
                       (point))))
           (start (and at-pos (1+ at-pos)))
           (valid (and start
                       (<= start pt)
                       ;; Между '@' и точкой не должно быть пробелов
                       (not (string-match-p "\\s-"
                                            (buffer-substring-no-properties start pt))))))
      (when valid
        (let* ((alist (pro/telega--mention-alist))
               (table
                (completion-table-dynamic
                 (lambda (str)
                   (let* ((case-fold-search true)
                          (str-no-at (if (and str (> (length str) 0) (eq (aref str 0) ?@))
                                         (substring str 1)
                                       str)))
                     (cl-loop for (uname . full) in alist
                              for cand = (or uname full)
                              when (and cand
                                        (if uname
                                            ;; Подбор по никнейму
                                            (string-match-p (regexp-quote str) uname)
                                          ;; Подбор по имени (после '@' ищем по имени)
                                          (string-match-p (regexp-quote str-no-at) full)))
                              collect (propertize cand
                                                  'telega-username-p (and uname t)
                                                  'telega-fullname full)))))))
          ;; Вставляем ТЕКСТ без '@'; символ '@' уже есть в буфере перед START
          (list start pt table
                :annotation-function
                (lambda (cand)
                  (let* ((is-user (get-text-property 0 'telega-username-p cand))
                         (full    (get-text-property 0 'telega-fullname cand)))
                    (cond
                     (is-user (concat " @" cand (when full (concat " — " full))))
                     (full    (concat " " full))
                     (t nil))))
                :exclusive 'no))))))

(defun pro/telega-enable-mention-capf ()
  "Включить автодополнение @упоминаний в telega чатах."
  (add-hook 'completion-at-point-functions #'pro/telega-mention-capf nil t)
  ;; Гарантируем, что TAB вызывает CAPF в чатах
  (local-set-key (kbd "TAB") #'completion-at-point)
  (local-set-key [tab] #'completion-at-point))

(with-eval-after-load 'telega
  (add-hook 'telega-chat-mode-hook #'pro/telega-enable-mention-capf))

;;;; 0. Введение и зависимости

;; Базовые зависимости времени выполнения
(require 'cl-lib)
(require 'subr-x)

;; Во время компиляции тоже подстрахуемся
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Не тянем тяжелые зависимости сразу; подгружаем их, когда команда вызвана.
;; Но типы символов/байндинги мы ссылаем опционально.
;; consult и telega должны быть установлены (MELPA / внешние источники).

;;;; 1. Пользовательская группа настроек и опции

(defgroup pro/интернет nil
  "Настройки функций взаимодействия consult + telega."
  :group 'applications
  :prefix "pro/telega-")

(defcustom pro/telega-select-preview 'echo
  "Режим предпросмотра кандидата:
- echo: печь краткую подсказку в echo-area;
- help-window: показывать описания (те же, что telega-describe-*);
- nil: предпросмотр выключен."
  :type '(choice (const :tag "Echo area" echo)
                 (const :tag "Help window" help-window)
                 (const :tag "Disabled" nil))
  :group 'pro/интернет)

(defcustom pro/telega-select-include-saved-messages t
  "Включать ли чат «Saved Messages» в начало списка (если он уже существует).
Если чат пока не создан — мы его не создаём автоматически."
  :type 'boolean
  :group 'pro/интернет)

(defcustom pro/telega-select-include-archived t
  "Включать ли архивные чаты.
Замечание: мы используем telega-temex '(or is-known has-chatbuf), что охватывает
в т.ч. архив. Отключение в текущей версии — логическое желание, но темекс
оставляем как есть ради предсказуемости. Опция остаётся для будущей настройки."
  :type 'boolean
  :group 'pro/интернет)

(defcustom pro/telega-select-show-unread t
  "Показывать ли краткую метку непрочитанного (u, @, rx) в аннотации чатов."
  :type 'boolean
  :group 'pro/интернет)

;; Хистори для minibuffer-выборов через consult
(defvar pro/consult-telega-history nil
  "История ввода для =pro/telega-select-chat-or-contact'.")

;;;; 2. Вспомогательные функции построения данных

(defun pro/telega--ensure-telega ()
  "Проверить наличие и готовность telega. Сообщить пользователю, если не запущено."
  (unless (require 'telega nil t)
    (user-error "Пакет telega не найден, установите его из MELPA"))
  (if (and (fboundp 'telega-server-live-p)
           (not (telega-server-live-p)))
      (user-error "telega не запущена (M-x telega)")))

(defun pro/telega--ensure-consult ()
  "Проверить наличие consult."
  (unless (require 'consult nil t)
    (user-error "Пакет consult не найден, установите его из MELPA")))

(defun pro/telega--contacts ()
  "Вернуть список пользователей-‘контактов’ (telega).
Это совместимо с текущим состоянием telega-user-list/temex."
  (cl-remove-if-not
   (lambda (u) (telega-user-match-p u 'contact))
   (ignore-errors (telega-user-list))))

(defun pro/telega--chat-choices ()
  "Вернуть список чатов для выбора.
Берём «известные» (Main/Archive) и те, у кого есть открытый chatbuf.
Применяем сортировку telega."
  (let* ((all (ignore-errors
                (telega-filter-chats (telega-chats-list)
                  '(or is-known has-chatbuf)))))
    (telega-sort-chats telega-chat-completing-sort-criteria all)))

(defun pro/telega--user-choices (existing-chats)
  "Вернуть список контактов, исключая тех, у кого уже есть приватный чат в EXISTING-CHATS."
  (let* ((chats-set (let ((ht (make-hash-table :test 'eq)))
                      (dolist (c existing-chats)
                        (puthash (plist-get c :id) t ht))
                      ht)))
    (cl-remove-if
     (lambda (u)
       (when-let ((chat (telega-user-chat u)))
         (gethash (plist-get chat :id) chats-set)))
     (sort (pro/telega--contacts) #'telega-user>))))

(defun pro/telega--saved-messages-chat ()
  "Вернуть чат «Saved Messages», если он уже существует (не создавая новый)."
  (ignore-errors (telega-chat-get telega--me-id 'offline)))

(defun pro/telega--unread-brief (chat)
  "Короткое строковое представление непрочитанного для CHAT."
  (let ((u (plist-get chat :unread_count))
        (m (plist-get chat :unread_mention_count))
        (r (plist-get chat :unread_reaction_count)))
    (string-join
     (delq nil
           (list (unless (zerop u) (format "u:%d" u))
                 (unless (zerop m) (format "@:%d" m))
                 (unless (zerop r) (format "rx:%d" r))))
     " ")))

(defun pro/telega--display-for-chat (chat)
  "Форматированная строка кандидата для CHAT.
Используем стандартный «telega-chat для completion» для стабильности визуализации."
  (concat (telega-msg-sender-title-for-completion chat) "  [chat]"))

(defun pro/telega--display-for-user (user)
  "Форматированная строка кандидата для USER."
  (concat (telega-msg-sender-title-for-completion user) "  [contact]"))

(defun pro/telega--annotator (alist)
  "Построить функция-аннотацию consult :annotate для ALIST вида:
  (DISPLAY . (:type TYPE :obj OBJ))."
  (lambda (cand)
    (when-let* ((entry (assoc cand alist))
                (type (plist-get (cdr entry) :type))
                (obj (plist-get (cdr entry) :obj)))
      (pcase type
        ('chat
         (if (and pro/telega-select-show-unread obj)
             (let ((unread (pro/telega--unread-brief obj)))
               (concat " [chat]" (unless (string-empty-p unread) (concat " " unread))))
           " [chat]"))
        ('user " [contact]")))))

(defun pro/telega--narrow (alist)
  "Сгенерировать :narrow spec (u — users, c — chats) для consult по ALIST."
  (let ((tbl (make-hash-table :test 'equal)))
    (dolist (e alist)
      (puthash (car e) (plist-get (cdr e) :type) tbl))
    (list
     (cons ?u (lambda (cand) (eq (gethash cand tbl) 'user)))
     (cons ?c (lambda (cand) (eq (gethash cand tbl) 'chat))))))

(defun pro/telega--preview-state (alist)
  "Функция :state для consult предпросмотра кандидатов в соответствии с pro/telega-select-preview.
ALIST — (DISPLAY . (:type TYPE :obj OBJ))."
  (let ((last nil))
    (lambda (action cand)
      (pcase action
        ('preview
         (setq last cand)
         (when-let* ((entry (assoc cand alist))
                     (type (plist-get (cdr entry) :type))
                     (obj  (plist-get (cdr entry) :obj)))
           (pcase pro/telega-select-preview
             ('echo
              (pcase type
                ('chat (message "Chat: %s  %s"
                                (telega-chat-title obj 'no-badges)
                                (pro/telega--unread-brief obj)))
                ('user (message "User: %s"
                                (telega-user-title obj 'full-name 'no-badges)))))
             ('help-window
              (pcase type
                ('chat (telega-describe-chat obj))
                ('user (telega-describe-user obj))))
             (_ nil)))
         )
        ((or 'return 'exit)
         (when last (message nil)))))))

;;;; 3. Построение списка кандидатов

(defun pro/telega--build-candidates (mode)
  "Собрать кандидатов для выбора. MODE — один из:
  - both: чаты и контакты;
  - chat-only: только чаты;
  - user-only: только контакты.

Возвращает (choices-nice . choices-alist), где:
- choices-nice — список отображаемых строк для consult;
- choices-alist — аллист DISPLAY -> plist (:type TYPE :obj OBJ)."
  (let* ((chats (pro/telega--chat-choices))
         (users (pro/telega--user-choices chats))
         (acc '()))

    ;; 3.1 Saved Messages — опционально, добавляем в голову списка чатов
    (when (and pro/telega-select-include-saved-messages
               (memq mode '(both chat-only)))
      (when-let ((sm (pro/telega--saved-messages-chat)))
        (push (cons (pro/telega--display-for-chat sm)
                    (list :type 'chat :obj sm))
              acc)))

    ;; 3.2 Чаты
    (when (memq mode '(both chat-only))
      (dolist (chat chats)
        (push (cons (pro/telega--display-for-chat chat)
                    (list :type 'chat :obj chat))
              acc)))

    ;; 3.3 Пользователи
    (when (memq mode '(both user-only))
      (dolist (user users)
        (push (cons (pro/telega--display-for-user user)
                    (list :type 'user :obj user))
              acc)))

    ;; Порядок строили «сверху вниз» через push — перевернём
    (setq acc (nreverse acc))

    (cons (mapcar #'car acc) acc)))

;;;; 4. Главная команда выбора

;;;###autoload
(defun pro/telega-select-chat-or-contact (&optional arg)
  "Выбрать чат/группу/канал или контакт через consult.
С префиксом ARG:
- C-u     — только чаты;
- C-u C-u — только контакты.
Без префикса — оба вида.

Предпросмотр настраивается переменной `pro/telega-select-preview'."
  (interactive "P")
  (pro/telega--ensure-consult)
  (pro/telega--ensure-telega)

  (let* ((mode (cond
                ((equal arg '(4))  'chat-only)
                ((equal arg '(16)) 'user-only)
                (t 'both)))
         (cdata (pro/telega--build-candidates mode))
         (choices (car cdata))
         (alist   (cdr cdata))
         (annot   (pro/telega--annotator alist))
         (narrow  (pro/telega--narrow alist))
         (state   (pro/telega--preview-state alist))
         (prompt  (pcase mode
                    ('chat-only "Telega: чат: ")
                    ('user-only "Telega: контакт: ")
                    (_          "Telega: чат/контакт: ")))
         (selected
          (consult--read choices
                         :prompt prompt
                         :require-match t
                         :history 'pro/consult-telega-history
                         :category 'unicode-name
                         :annotate annot
                         :narrow narrow
                         :sort t
                         :state state))
         (entry (assoc selected alist))
         (type (plist-get (cdr entry) :type))
         (obj  (plist-get (cdr entry) :obj)))
    (pcase type
      ('chat (telega-chat--pop-to-buffer obj))
      ('user (telega-user-chat-with obj)))))

;;;; 5. Совместимость и вспомогательные алиасы

;;;###autoload
(defalias 'pro/telega-select-contact #'pro/telega-select-chat-or-contact)

;;;; 6. Экспортируем фичу

(provide 'про-интернет-общение)

;;; про-интернет-общение.el ends here
