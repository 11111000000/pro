;;; init.el --- init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst папка-про (expand-file-name "~/pro"))
(add-to-list 'load-path папка-про)

(when (fboundp 'pro/early-log)
  (pro/early-log "init" "sample-init start"))

(message "sample-init: start")

;; Добавить все подпапки в load-path для доступа к модулям
(dolist (dir (directory-files папка-про t))
  (when (and (file-directory-p dir)
             (not (string-match "\\`\\." (file-name-nondirectory dir))))
    (add-to-list 'load-path dir)))

;; Загрузить auth-source для работы с ~/.authinfo
(require 'auth-source nil t)

;; Прочие специальные настройки
(when (fboundp 'pro/early-log)
  (pro/early-log "init" "loading other.el"))
(message "sample-init: loading other.el")
(load-file (expand-file-name "~/.emacs.d/other.el"))
(when (fboundp 'pro/early-log)
  (pro/early-log "init" "loaded other.el"))
;; Прочие специальные настройки
(when (fboundp 'pro/early-log)
  (pro/early-log "init" "loading feeds.el"))
(message "sample-init: loading feeds.el")
(load-file (expand-file-name "~/.emacs.d/feeds.el"))

(when (fboundp 'pro/early-log)
  (pro/early-log "init" "loaded feeds.el"))

(when (fboundp 'pro/early-log)
  (pro/early-log "init" "requiring zagrusit"))
(message "sample-init: requiring загрузить")
(require 'загрузить)
(when (fboundp 'pro/early-log)
  (pro/early-log "init" "requiring pro-otladku"))
(message "sample-init: requiring про-отладку")
(require 'про-отладку)
(pro/log-startup-stage "init" "sample-init.el loaded")

;; Emacs 30.2 needs this prebound before package bootstrap.
(defvar url-honor-refresh-requests nil)

(defvar pro/startup-modules
  '(про-оптимизацию
    ;;(загрузить 'про-отладку)
    про-менеджер-пакетов
    про-шифрование
    ;;(загрузить 'про-цвет)
    ;; (load-theme 'purp t)
    про-текстовый-режим
    про-функции
    про-клавиши-из-org
    про-графическую-среду-старт
    про-внешний-вид
    про-историю
    про-буферы
    про-редактор
    про-окна
    про-доску
    про-файлы-и-папки
    про-организацию
    про-терминалы
    про-nix
    про-код
    про-код-на-lisp
    про-автодополнение
    про-фикс-corfu
    про-быстрый-доступ
    про-код-на-javascript
    про-код-на-rust
    про-управление-проектами
    про-словари-и-перевод
    про-интернет-общение
    про-интернет-сервисы
    про-новости
    про-инструменты
    про-ии-ядро
    про-справку
    про-устройства
    про-малую-механизацию)
  "Список стартовых модулей для e2e запуска.")

(dolist (feature pro/startup-modules)
  (pro/log-startup-stage "module-begin" (format "%s" feature))
  (pro/load-module feature))
