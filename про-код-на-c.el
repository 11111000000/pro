;;; про-код-на-c.el --- Поддержка C# в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: csharp, treesitter, nix
;; URL: https://example.com/про-код-на-c
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку C# в Emacs, следуя принципам литературного
;; программирования: код представлен как повествование, где каждая
;; секция мотивируется, объясняется и логически связывается с остальными. Мы
;; стремимся к элегантности, минимализму и производительности.
;;
;; Почему это важно? C# — ключевой язык для .NET-разработки. Здесь мы используем
;; csharp-mode с tree-sitter для точной подсветки и добавляем Nix-поддержку
;; для компиляции в изолированных окружениях.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require (если нужны).
;; 1. Csharp-mode: Базовая настройка с Nix-интеграцией.
;; 2. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-код-на-c) в вашем init.el.
;; Рекомендуется с Eglot для LSP.
;;
;; Замечания: Nix-хук локальный для проектов с shell.nix.

;;; Code:

;;;; 1. Csharp-mode
;; Csharp-mode предоставляет режим для .cs-файлов с tree-sitter. Мы добавляем
;; ассоциацию и Nix-поддержку для компиляции через nix-shell, обеспечивая
;; воспроизводимость в проектах.

(use-package csharp-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  ;; Поддержка Nix: если shell.nix, использовать nix-shell для компиляции/загрузки через Eglot.
  (when (and (executable-find "nix-shell") (file-exists-p "shell.nix"))
    (add-hook 'csharp-mode-hook
              (lambda ()
                (setq-local compile-command "nix-shell --run 'mcs'")))))

;;;; 2. Финал
;; Завершаем модуль.

(provide 'про-код-на-c)
;;; про-код-на-c.el ends here
