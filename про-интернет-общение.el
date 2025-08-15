;;; про-интернет-общение.el --- общение в интернет -*- lexical-binding: t -*-
;;; Commentary:
;; Конфигурация мессенджеров
;;; Code:

;;;; Telegram

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
           (telega-webpage-preview-mode nil)
           (telega-chat-fill-column 80)
           (telega-chat-history-limit 100)
           ;; (telega-emoji-use-images t)
           (telega-emoji-font-family "Noto Color Emoji"))
  :hook ((telega-root-mode . telega-notifications-mode)
         (telega-load-hook . global-telega-url-shorten-mode)
         (telega-root-mode . hl-line-mode)
                                        ;(telega-chat-mode . variable-pitch-mode)
         )
  :config)

(defun telega-close-idle-chat-buffers ()
  "Автоматически закрывает неактивные telega-буферы."
  (interactive)
  (dolist (buf (buffer-list))
    (when (string-match-p "^\\*Telega Chat" (buffer-name buf))
      (kill-buffer buf))))

(defun выбрать-контакт-в-телеге ()
  "Выбор из всех контактов и чатов Telega с помощью consult."
  (interactive)
  (require 'telega)
  (require 'consult)
  (let* ((contacts (telega--getContacts))
         (formatted-contacts (mapcar (lambda (user)
                                       (cons (telega--tl-get user :first_name) user))
                                     contacts)))
    (consult--read (mapcar 'car formatted-contacts)
                   :prompt "Выберите контакт или чат: "
                   :category 'telega-contact-chat
                   :require-match t
                   :history 'consult--telega-contacts-chats-history
                   :sort t
                   :state (lambda (action selected)
                            (when selected
                              (let ((choice (cdr (assoc selected formatted-contacts))))
                                (telega-chat-with choice)))))))

;; Автодополнение @упоминаний в telega по нику и имени (в т.ч. кириллица).
(require 'cl-lib)
(require 'subr-x)

(defun az/telega--contacts ()
  "Вернуть список объектов user для всех контактов."
  (require 'telega)
  (let ((contacts (telega--getContacts)))
    (cond
     ((vectorp contacts) (append contacts nil))
     ((listp contacts) contacts)
     (t nil))))

(defun az/telega--mention-alist ()
  "Вернуть alist (username . fullname) для контактов.
fullname — склеенные first_name и last_name (может быть nil/пустым).
Если у пользователя несколько активных никнеймов — вернуть все."
  (cl-loop for u in (az/telega--contacts)
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

(defun az/telega-mention-capf ()
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
        (let* ((alist (az/telega--mention-alist))
               (table
                (completion-table-dynamic
                 (lambda (str)
                   (let* ((case-fold-search t)
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

(defun az/telega-enable-mention-capf ()
  "Включить автодополнение @упоминаний в telega чатах."
  (add-hook 'completion-at-point-functions #'az/telega-mention-capf nil t)
  ;; Гарантируем, что TAB вызывает CAPF в чатах
  (local-set-key (kbd "TAB") #'completion-at-point)
  (local-set-key [tab] #'completion-at-point))

(with-eval-after-load 'telega
  (add-hook 'telega-chat-mode-hook #'az/telega-enable-mention-capf))

(provide 'про-интернет-общение)
;;; про-интернет-общение.el ends here
