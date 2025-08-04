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
;; функции `про/enable-debug` и `про/disable-debug` (см. конец файла).
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

;;;; 3. Улучшенный вывод backtrace

;; При ошибке в adviced-функциях показывать полный стек с advice.
(setq debugger-stack-frame-as-list t)

;; Edebug: показывать исходник прямо в backtrace.
(setq edebug-print-level  4
      edebug-print-length 150)

;;;; 4. Интерактивные переключатели

(defun про/enable-debug ()
  "Включить основные опции отладки целиком."
  (interactive)
  (setq debug-on-error  t
        debug-on-signal t
        debug-on-quit   t
        warning-minimum-level :debug)
  (message "Debug: ON"))

(defun про/disable-debug ()
  "Полностью выключить опции отладки."
  (interactive)
  (setq debug-on-error  nil
        debug-on-signal nil
        debug-on-quit   nil
        warning-minimum-level :error)
  (message "Debug: OFF"))

(provide 'про-отладку)
;;; про-отладку.el ends here
