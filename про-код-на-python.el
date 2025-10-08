;;; про-код-на-python.el --- Поддержка Python в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: python, pyvenv, virtualenv, nix
;; URL: https://example.com/про-код-на-python
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку Python в Emacs, следуя принципам литературного
;; программирования: код представлен как повествование, где каждая
;; секция мотивируется, объясняется и логически связывается с остальными. Мы
;; стремимся к элегантности, минимализму и производительности в лучших традициях
;; Emacs — с использованием `use-package` для декларативной конфигурации.
;;
;; Почему это важно? Python — универсальный язык для скриптинга и data science.
;; Здесь мы фокусируемся на управлении виртуальными окружениями (pyvenv, auto-virtualenv)
;; и интеграции с Nix через envrc, обеспечивая изоляцию и воспроизводимость.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require (если нужны).
;; 1. Pyvenv: Базовое управление виртуальными окружениями.
;; 2. Auto-virtualenv: Автоматическая активация.
;; 3. Envrc: Nix-интеграция для Python-окружений.
;; 4. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-код-на-python) в вашем init.el.
;; Рекомендуется с Eglot для LSP.
;;
;; Замечания: Мы используем хуки для автоматизации.

;;; Code:

;;;; 1. Pyvenv
;; Pyvenv управляет виртуальными окружениями, устанавливая правильный интерпретатор.
;; Мы настраиваем хуки для активации/деактивации, обеспечивая seamless переключение.

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

;;;; 2. Auto-virtualenv
;; Auto-virtualenv автоматически обнаруживает и активирует venv в проектах,
;; добавляя удобство без ручной настройки, с verbose для отладки.

(use-package auto-virtualenv
  :ensure t
  :config
  (setq auto-virtualenv-verbose t)
  (auto-virtualenv-setup))

;;;; 3. Envrc
;; Envrc интегрирует direnv для Nix-окружений, автоматически загружая shell.nix
;; в python-mode. Это связывает Python с Nix, мотивируя использование в контейнеризованных проектах.

;; Поддержка Nix: интеграция с direnv/envrc для Nix-окружений, где есть shell.nix с Python.
(use-package envrc
  :if (executable-find "direnv")
  :hook (python-mode . envrc-allow)
  :config
  (envrc-global-mode))

(provide 'про-код-на-python)
;;; про-код-на-python.el ends here
