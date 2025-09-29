;;; про-отладку.el --- Включаем все флаги отладки Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 0.1
;; Keywords: debug, lisp
;; URL: https://example.com/про-отладку
;;
;;; Commentary:
;;
;; Этот файл собирает воедино самые полезные «рубильники» для отладки Emacs
;; ― от классических debug-on-error / debug-on-signal до более тонких
;; предупреждений, логов и интроспекции.  Подключите:
;;
;;   (require 'про-отладку)
;;
;; или добавьте в init-файл:
;;
;;   (load "~/pro/про-отладку.el")
;;
;; Чтобы оперативно включать/выключать режимы, предусмотрены интерактивные
;; функции `pro/enable-debug` и `pro/disable-debug` (см. конец файла).
;;
;;; Code:

;;;; 1. Базовые флаги отладки

;; Падать в *debug* при любой непойманной ошибке или сигнале.
(setq debug-on-error  t          ; любой `error'
      debug-on-signal t          ; любые non-error ситуации (quit, etc.)
      debug-on-quit   t)         ; C-g тоже покажет стек

;; Раскрыть предупреждения до уровня :debug, чтобы ничего не пряталось.
(setq warning-minimum-level :debug)

;;;; 2. Расширенное логирование сообщений

;; Запоминаем последние 2000 сообщений вместо стандартных 100.
(setq message-log-max 2000)

;; Автоматически открывать *Messages* при старте Emacs (удобно для журналов).
(add-hook 'emacs-startup-hook
          (lambda ()
            (when-let ((buf (get-buffer "*Messages*")))
              (display-buffer buf))))

;; Писать сообщения/предупреждения и метки времени в файл ~/.emacs.d/emacs.log.
(defvar pro/log-file (expand-file-name "emacs.log" user-emacs-directory)
  "Файл для логирования сообщений и предупреждений Emacs.")

(defun pro/log--append-line (line)
  "Append LINE to `pro/log-file' with a timestamp."
  (when (and (stringp line) (> (length line) 0))
    (let* ((ts (format-time-string "%Y-%m-%d %H:%M:%S%z"))
           (text (format "[%s] %s\n" ts line))
           (coding-system-for-write 'utf-8))
      (let ((debug-on-error nil)
            (debug-on-signal nil))
        (condition-case _err
            (let ((dir (file-name-directory pro/log-file)))
              (unless (file-directory-p dir)
                (make-directory dir t))
              (with-temp-buffer
                (insert (substring-no-properties text))
                (write-region (point-min) (point-max) pro/log-file 'append 'silent)))
          (error nil))))))

(defun pro/log-message-advice (orig format-string &rest args)
  "Advice to also log `message' output to `pro/log-file'."
  (if (null format-string)
      (apply orig format-string args)
    (let ((out (apply orig format-string args)))
      (pro/log--append-line out)
      out)))

(defun pro/log-display-warning-advice (orig type message &optional level buffer-name)
  "Advice to also log `display-warning' output to `pro/log-file'."
  (prog1 (funcall orig type message level buffer-name)
    (pro/log--append-line
     (format "WARNING[%s/%s]: %s"
             type (or level :warning) message))))

(defun pro/enable-file-log ()
  "Включить запись сообщений/предупреждений в `pro/log-file'."
  (interactive)
  (advice-remove 'message #'pro/log-message-advice)
  (advice-remove 'display-warning #'pro/log-display-warning-advice)
  (advice-add 'message :around #'pro/log-message-advice)
  (advice-add 'display-warning :around #'pro/log-display-warning-advice))

(defun pro/disable-file-log ()
  "Выключить запись сообщений/предупреждений в `pro/log-file'."
  (interactive)
  (advice-remove 'message #'pro/log-message-advice)
  (advice-remove 'display-warning #'pro/log-display-warning-advice))

;; Включаем логирование сразу при загрузке этого файла.
(pro/enable-file-log)

;;;; 3. Улучшенный вывод backtrace

;; При ошибке в adviced-функциях показывать полный стек с advice.
(setq debugger-stack-frame-as-list t)

;; Edebug: показывать исходник прямо в backtrace.
(setq edebug-print-level  4
      edebug-print-length 150)

;;;; 4. Интерактивные переключатели

(defun pro/enable-debug ()
  "Включить основные опции отладки целиком."
  (interactive)
  (setq debug-on-error  t
        debug-on-signal t
        debug-on-quit   t
        warning-minimum-level :debug)
  (message "Debug: ON"))

(defun pro/disable-debug ()
  "Полностью выключить опции отладки."
  (interactive)
  (setq debug-on-error  nil
        debug-on-signal nil
        debug-on-quit   nil
        warning-minimum-level :error)
  (message "Debug: OFF"))

(provide 'про-отладку)
;;; про-отладку.el ends here
