;;; init.el --- init.el  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defconst папка-про (expand-file-name "~/pro"))
(add-to-list 'load-path папка-про)

;; Добавить все подпапки в load-path для доступа к модулям
(dolist (dir (directory-files папка-про t))
  (when (and (file-directory-p dir)
             (not (string-match "\\`\\." (file-name-nondirectory dir))))
    (add-to-list 'load-path dir)))

;; Загрузить auth-source для работы с ~/.authinfo
(require 'auth-source nil t)

;; Прочие специальные настройки
(load-file (expand-file-name "~/.emacs.d/other.el"))
;; Прочие специальные настройки
(load-file (expand-file-name "~/.emacs.d/feeds.el"))

(require 'загрузить)
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
    про-графическую-среду
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
  (pro/load-module feature))
