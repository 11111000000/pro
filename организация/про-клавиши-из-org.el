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
(require 'cl-lib)
(require 'exwm)  ;; Если используется EXWM, иначе закомментировать

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
        (mapc (lambda (row)
                (when (consp row)
                  (cl-destructuring-bind (key func) row
                    (global-set-key (kbd key) (intern func)))))
              keys-table)))

    ;; Load exwm-key-bindings-table (global + EXWM if available)
    (let ((exwm-keys-table (org-get-named-table "exwm-key-bindings-table")))
      (when exwm-keys-table
        (mapc (lambda (row)
                (when (consp row)
                  (cl-destructuring-bind (key func) row
                    (global-set-key (kbd key) (intern func)))))
              exwm-keys-table)
        ;; EXWM-specific bindings if in window-system and function exists
        (when (and window-system (functionp 'exwm-input-set-key))
          (mapc (lambda (row)
                  (when (consp row)
                    (cl-destructuring-bind (key func) row
                      (exwm-input-set-key (kbd key) (intern func)))))
                exwm-keys-table))))

    ;; Load modes-key-bindings-table (mode-specific)
    (let ((modes-table (org-get-named-table "modes-key-bindings-table")))
      (when modes-table
        (mapc (lambda (row)
                (when (consp row)
                  (cl-destructuring-bind (mode key func) row
                    (let ((mode-map (intern (concat mode "-map"))))
                      (when (boundp mode-map)
                        (define-key (symbol-value mode-map) (kbd key) (intern func)))))))
              modes-table)))))

;; Пример использования: (load-keybindings-from-org "~/pro/про-сочетания-клавиш.org")
(defun pro/reload-keys ()
  "Перезагрузить клавиши из файла `~/pro/про-клавиши.org`."
  (interactive)
  (pro/клавиши-из-org (expand-file-name "~/pro/про-клавиши.org"))
  (message "Клавиши перезагружены"))


(provide 'про-клавиши-из-org)
