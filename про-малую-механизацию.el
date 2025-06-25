;;; про-малую-механизацию.el --- Настройки малой механизации Emacs  -*- lexical-binding: t -*-
;;; Commentary:
;; Функции для упрощённой настройки Emacs, в том числе быстрая привязка клавиш.

;;; Code:
(defun про/добавь-кнопку (&optional арг)
  "Быстрое создание глобального биндинга.
Пользователь вводит сочетание клавиш, выбирает функцию,
биндинг добавляется в ~/.emacs.d/keys.el.
Если вызывать с C-u (универсальный аргумент) — добавляет также
глобальный exwm-биндинг."
  (interactive "P")
  ;; 1. Получить клавишу и имя в человеко-читаемом виде
  (let* ((key-vec (read-key-sequence-vector "Введите сочетание клавиш: "))
         (pretty-key (key-description key-vec)))
    ;; Сообщить, что выбрано
    (message "Вы выбрали: %s" pretty-key)
    ;; 2. Переместить курсор в minibuffer (если активен)
    (when (active-minibuffer-window)
      (select-window (active-minibuffer-window)))
    ;; 3. Запросить функцию
    (let* ((fn-symbol
            (intern
             (if (require 'consult nil t)
                 (consult--read
                  (let (candidates)
                    (mapatoms
                     (lambda (s)
                       (when (and (commandp s)
                                  (not (string-prefix-p "mouse" (symbol-name s))))
                         (push (symbol-name s) candidates))))
                    candidates)
                  :prompt (format "Связать функцию с <%s>: " pretty-key)
                  :require-match t
                  :category 'command)
               (completing-read (format "Связать функцию с <%s>: " pretty-key)
                                obarray 'commandp t))))
           (keys-file (expand-file-name "keys.el" user-emacs-directory))
           (bind-string
            (format "(global-set-key (kbd \"%s\") '%s)\n" pretty-key fn-symbol))
           (exwm-bind-string
            (format "(when (featurep 'exwm-input) (exwm-input-set-key (kbd \"%s\") '%s))\n"
                    pretty-key fn-symbol))
           (full-bind-string
            (if арг
                (concat bind-string exwm-bind-string)
              bind-string)))
      ;; Прямо здесь делать глобальную привязку
      (global-set-key (kbd pretty-key) fn-symbol)
      ;; Если был универсальный аргумент — пробуем и EXWM привязку
      (when арг
        (when (featurep 'exwm-input)
          (exwm-input-set-key (kbd pretty-key) fn-symbol)))
      ;; Вносим в файл
      (with-temp-buffer
        (when (file-exists-p keys-file)
          (insert-file-contents keys-file))
        (goto-char (point-max))
        (insert full-bind-string)
        (write-region (point-min) (point-max) keys-file))
      (if арг
          (message "Привязки %s -> %s (и EXWM) добавлены в %s" pretty-key fn-symbol keys-file)
        (message "Привязка %s -> %s добавлена в %s" pretty-key fn-symbol keys-file)))))


;; Глобальные пользовательские клавиши (keys.el)
(let ((user-keys-file (expand-file-name "keys.el" user-emacs-directory)))
  (when (file-exists-p user-keys-file)
    (load user-keys-file nil t)))

(provide 'про-малую-механизацию)
;;; про-малую-механизацию.el ends here.
