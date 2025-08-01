;;; про-клавиши-из-org.el --- Загрузить клавиши -*- lexical-binding: t -*-

;; Функция =load-keybindings-from-org= принимает путь к Org-файлу (например, "~/path/to/file.org"), парсит указанные named таблицы и устанавливает keybindings соответственно:
;; - =key-bindings-table=: Глобальные привязки (global-set-key).
;; - =exwm-key-bindings-table=: Привязки для EXWM (global-set-key + exwm-input-set-key, если доступно).
;; - =modes-key-bindings-table=: Привязки для конкретных режимов (define-key в mode-map).

;; Функция использует временный буфер для чтения файла, парсит таблицы с помощью =org-table-to-lisp= и игнорирует заголовочные строки. Предполагается, что таблицы имеют указанные структуры (без учета локализации заголовков, как "Сочетание" или "Мод").


(require 'org)
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

(defun про/клавиши-из-org (filename)
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
(defun про/перезагрузить-клавиши ()
  "Перезагрузить клавиши из файла `~/pro/про-клавиши.org`."
  (interactive)
  (про/клавиши-из-org (expand-file-name "~/pro/про-клавиши.org"))
  (message "Клавиши перезагружены"))


(provide 'про-клавиши-из-org)
