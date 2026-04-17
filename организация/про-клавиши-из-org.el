;;; про-клавиши-из-org.el --- Загрузка клавиш из Org-таблиц -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: keybinding, org, table, exwm
;; URL: https://github.com/username/emacs.d/blob/main/организация/про-клавиши-из-org.el
;;
;;; Commentary:
;;
;; Этот файл загружает клавишные привязки из Org-таблиц, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Хранение биндов в Org-таблицах позволяет легко
;; редактировать и поддерживать клавиши. Функция парсит таблицы:
;; - key-bindings-table: глобальные привязки
;; - exwm-key-bindings-table: привязки для EXWM
;; - modes-key-bindings-table: привязки для режимов
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Функция поиска таблицы
;;  2. Парсинг таблиц и установка биндов
;;  3. Глобальные бинды
;;  4. EXWM-бинды
;;  5. Бинды для режимов
;;  6. Финал: Provide и ends here
;;
;; Использование: (pro/клавиши-из-org "~/path/to/keys.org")
;;
;;; Code:
(condition-case err
    (progn
      (require 'cl-lib)
      (require 'exwm nil t))
  (error
   (message "Ошибка require в про-клавиши-из-org: %s" err)))

(defun org-get-named-table (name)
  "Extract a named Org table as Lisp list from current buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "#\\+NAME: ?%s" name) nil t)
      (forward-line 1)  ;; Skip to the table start
      (let ((table (org-table-to-lisp)))
        (setq table (cl-remove-if (lambda (x) (eq x 'hline)) table))
        ;; Remove header row if it's a string list (assuming first row is header)
        (if (and table (consp (car table)) (stringp (caar table)))
            (cdr table)
          table)))))

(defun pro/клавиши-из-org (filename)
  "Load and set keybindings from named tables in the given Org file."
  (interactive "Org file: ")
  (with-temp-buffer
    (insert-file-contents filename)
    (org-mode)

    ;; Load global key-bindings-table
    (let ((keys-table (org-get-named-table "key-bindings-table")))
      (when keys-table
        (dolist (row keys-table)
          (when (consp row)
            (cl-destructuring-bind (key func) row
              (global-set-key (kbd key) (intern func))
              ;; TTY support: add explicit binding for ESC after C-c
              (when (string-match-p "^C-c M-" key)
                (let* ((letter (substring key (length "C-c M-")))
                       (cmd (intern func)))
                  ;; Direct binding with explicit ESC key
                  (define-key global-map (kbd (format "C-c %c%s" ?\e letter)) cmd)
                  (message "TTY: bound C-c ESC%s -> %s" letter cmd)))))

          ;; Load exwm-key-bindings-table (global + EXWM if available)
          (let ((exwm-keys-table (org-get-named-table "exwm-key-bindings-table")))
            (when exwm-keys-table
              (dolist (row exwm-keys-table)
                (when (consp row)
                  (cl-destructuring-bind (key func) row
                    (global-set-key (kbd key) (intern func))
                    ;; TTY support for EXWM bindings too
                    (when (string-match-p "^C-c M-" key)
                      (let ((tty-key (replace-regexp-in-string "M-" "ESC" key)))
                        (global-set-key (kbd tty-key) (intern func)))))))
              ;; EXWM-specific bindings if in window-system and function exists
              (when (and window-system (functionp 'exwm-input-set-key))
                (dolist (row exwm-keys-table)
                  (when (consp row)
                    (cl-destructuring-bind (key func) row
                      (exwm-input-set-key (kbd key) (intern func))
                      ;; TTY support
                      (when (string-match-p "^C-c M-" key)
                        (let ((tty-key (replace-regexp-in-string "M-" "ESC" key)))
                          (exwm-input-set-key (kbd tty-key) (intern func)))))))))

            ;; Load modes-key-bindings-table (mode-specific)
            (let ((modes-table (org-get-named-table "modes-key-bindings-table")))
              (when modes-table
                (dolist (row modes-table)
                  (when (consp row)
                    (cl-destructuring-bind (mode key func) row
                      (let ((mode-map (intern (concat mode "-map"))))
                        (when (boundp mode-map)
                          (define-key (symbol-value mode-map) (kbd key) (intern func)))))))))))))))

(defalias 'pro/klavishy-iz-org #'pro/клавиши-из-org)

;; Пример использования: (load-keybindings-from-org "~/pro/про-сочетания-клавиш.org")
(defun pro/автозагрузка-клавиш ()
  "Автоматически загрузить клавиши из системного ~/.emacs.d/мои-клавиши.org или про-клавиши.org в корне pro."
  (let* ((системный-файл (expand-file-name "~/.emacs.d/мои-клавиши.org"))
         (локальный-файл (expand-file-name "~/pro/про-клавиши.org"))
         (выбранный-файл (cond
                          ((file-exists-p системный-файл) системный-файл)
                          ((file-exists-p локальный-файл) локальный-файл)
                          (t nil))))
    (when выбранный-файл
      (pro/клавиши-из-org выбранный-файл)
      (message "Загружены клавиши из: %s" выбранный-файл))))

(defun pro/reload-keys ()
  "Перезагрузить клавиши из файла `~/pro/про-клавиши.org`."
  (interactive)
  (pro/автозагрузка-клавиш))


;; Автоматически загрузить клавиши при загрузке модуля
(condition-case err
    (pro/автозагрузка-клавиш)
  (error
   (message "Ошибка загрузки клавиш: %s" err)))

(provide 'про-клавиши-из-org)
