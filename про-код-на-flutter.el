;;; про-код-на-flutter.el --- Поддержка Dart/Flutter в Emacs (Eglot + утилиты) -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: dart, flutter, eglot, lsp, mobile
;;
;;; Commentary:
;;
;; Минималистичная поддержка Dart/Flutter:
;; - Авто-режим для .dart файлов (dart-mode).
;; - Интеграция с Eglot через "dart language-server" (из Dart SDK).
;; - Автоформатирование перед сохранением через eglot-format-buffer.
;; - Команды Flutter (run, hot reload/restart, pub get, devtools, тесты) и удобные бинды.
;;
;; Требования:
;; - Установленный Dart SDK (команда =dart= в PATH).
;; - Установленный Flutter SDK (команда =flutter= в PATH) для flutter-команд.
;;
;; Пример использования:
;;   (require 'про-код-на-flutter)
;;
;;; Code:

(require 'use-package)

;; Dart major-mode + Eglot (LSP)
(use-package dart-mode
  :ensure t
  :mode "\\.dart\\'"
  :hook
  ((dart-mode . eglot-ensure)
   (dart-mode . (lambda ()
                  ;; Форматировать через LSP перед сохранением
                  (add-hook 'before-save-hook #'eglot-format-buffer nil t))))
  :init
  ;; Связать dart-mode с сервером LSP "dart language-server" из Dart SDK.
  ;; Нужен установленный =dart= (dart --version).
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(dart-mode . ("dart" "language-server")))))

;; Утилиты Flutter: run, hot-reload/restart, devtools, pub get, тесты
(use-package flutter
  :ensure t
  :after dart-mode
  :commands (flutter-run
             flutter-hot-reload
             flutter-hot-restart
             flutter-open-devtools
             flutter-pub-get
             flutter-test
             flutter-test-all
             flutter-test-current)
  :init
  ;; Набор удобных биндов под префиксом C-c C-f … в dart-буферах
  (with-eval-after-load 'dart-mode
    (let ((m dart-mode-map))
      (define-key m (kbd "C-c C-f r") #'flutter-run)
      (define-key m (kbd "C-c C-f h") #'flutter-hot-reload)
      (define-key m (kbd "C-c C-f R") #'flutter-hot-restart)
      (define-key m (kbd "C-c C-f d") #'flutter-open-devtools)
      (define-key m (kbd "C-c C-f p") #'flutter-pub-get)
      (define-key m (kbd "C-c C-f t") #'flutter-test)
      (define-key m (kbd "C-c C-f T") #'flutter-test-all)
      (define-key m (kbd "C-c C-f c") #'flutter-test-current))))

(provide 'про-код-на-flutter)
;;; про-код-на-flutter.el ends here
