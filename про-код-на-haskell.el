;;; про-код-на-haskell.el --- Поддержка Haskell в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.3
;; Keywords: haskell, eglot, cabal, ghci, formatting
;; URL: https://example.com/про-код-на-haskell
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку Haskell в Emacs, следуя принципам литературного
;; программирования: код представлен как повествование, где каждая
;; секция мотивируется, объясняется и логически связывается с остальными.  Мы
;; стремимся к элегантности, минимализму и производительности в лучших традициях
;; Emacs — с использованием `use-package` для декларативной конфигурации, хуков
;; для автоматизации и отложенной загрузки для скорости.
;;
;; Почему это важно? Haskell — функциональный язык с сильной типизацией, и
;; эффективная работа с ним усиливает Emacs как IDE. Здесь мы фокусируемся на
;; интеграции с LSP (Eglot) для автодополнения, REPL (GHCi) для интерактивности,
;; форматировании и отладке, делая Emacs мощным инструментом без излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и утилиты.
;; 1. Основной режим для Haskell: Haskell-mode с Eglot.
;; 2. REPL и интерактивность: Interactive Haskell с GHCi/Cabal.
;; 3. Форматирование и стиль: Автоматическое выравнивание кода.
;; 4. Отладка и инспекция: Декларации, типы и логи.
;; 5. Дополнительные инструменты: Decl-scan, doc-mode и сниппеты.
;; 6. Кастомные функции: Полезные утилиты для Haskell-разработки.
;; 7. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-код-на-haskell) в вашем init.el.
;; Рекомендуется интегрировать с Projectile для проектов Cabal/Stack. Закомментированные
;; секции — опции для экспериментов (например, альтернативные форматтеры).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Для конфликтов — проверяйте remap.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые пакеты. `use-package` обеспечивает модульность,
;; а `eglot` — LSP для Haskell. Это основа: без них дальнейшие секции не смогут
;; работать декларативно, интегрируя Emacs с внешними инструментами как HLS.

(require 'use-package)
(require 'eglot)

;;;; 1. Основной режим для Haskell
;; Haskell-mode — фундамент: предоставляет синтаксис, индентацию и навигацию.
;; Мы интегрируем Eglot для LSP (Haskell Language Server), что даёт автодополнение,
;; хинты и рефакторинг. Мотивация: Haskell требует сильной поддержки типов, и LSP
;; упрощает разработку, связывая Emacs с внешним сервером.

;; Настройка контакта Eglot для Haskell до запуска Eglot и с учётом Nix.
(defun pro/haskell-eglot-setup ()
  "Настроить локальный сервер Eglot для Haskell.
Если рядом в проекте есть flake.nix — использовать nix develop;
если shell.nix — использовать nix-shell."
  (let* ((flake-root (locate-dominating-file default-directory "flake.nix"))
         (shell-root (locate-dominating-file default-directory "shell.nix"))
         (in-flake (and (executable-find "nix") flake-root))
         (in-shell (and (executable-find "nix-shell") shell-root))
         (wrapper (executable-find "haskell-language-server-wrapper"))
         (hls (or wrapper (executable-find "haskell-language-server")))
         (modes '(haskell-mode haskell-literate-mode literate-haskell-mode haskell-debug-mode)))
    (cond
     (in-flake
      (dolist (mm modes)
        (setq-local eglot-server-programs
                    (cons
                     (cons mm (list "nix" "develop" flake-root "--command"
                                    "haskell-language-server-wrapper" "--lsp"))
                     (assq-delete-all mm eglot-server-programs)))))
     (in-shell
      (dolist (mm modes)
        (setq-local eglot-server-programs
                    (cons
                     (cons mm '("nix-shell" "-p" "haskell-language-server"
                                "--" "haskell-language-server-wrapper" "--lsp"))
                     (assq-delete-all mm eglot-server-programs)))))
     (hls
      (let ((cmd (if wrapper
                     '("haskell-language-server-wrapper" "--lsp")
                   '("haskell-language-server" "--lsp"))))
        (dolist (mm modes)
          (setq-local eglot-server-programs
                      (cons (cons mm cmd)
                            (assq-delete-all mm eglot-server-programs))))))
     (t
      ;; HLS не найден — оставляем как есть; pro/haskell-eglot-ensure пропустит запуск.
      ))))

(defun pro/haskell-eglot-ensure ()
  "Безопасно запустить Eglot в Haskell-буфере, только если доступен сервер."
  (let* ((in-flake (and (executable-find "nix")
                        (locate-dominating-file default-directory "flake.nix")))
         (in-shell (and (executable-find "nix-shell")
                        (locate-dominating-file default-directory "shell.nix")))
         (hls (or (executable-find "haskell-language-server-wrapper")
                  (executable-find "haskell-language-server"))))
    (if (or in-flake in-shell hls)
        (eglot-ensure)
      (message "HLS не найден в PATH и нет flake.nix/shell.nix — пропускаю eglot-ensure"))))

(use-package haskell-mode
  :ensure t
  :defer t
  :init
  ;; Базовая (не-Nix) привязка HLS — задаём как можно раньше.
  (add-to-list 'eglot-server-programs
               '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  ;; Поддержка literate Haskell.
  (add-to-list 'eglot-server-programs
               '(haskell-literate-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(literate-haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '(haskell-debug-mode . ("haskell-language-server-wrapper" "--lsp")))
  :hook ((haskell-mode . pro/haskell-eglot-setup)   ; Сначала контакт (Nix), если нужен.
         (haskell-mode . pro/haskell-eglot-ensure)  ; Затем — безопасный запуск Eglot.
         (haskell-mode . haskell-decl-scan-mode)
         (haskell-mode . haskell-doc-mode)
         (haskell-mode . interactive-haskell-mode)
         ;; (haskell-mode . subword-mode)
         ;; (haskell-mode . haskell-indentation-mode)
         (haskell-mode . yas-minor-mode))
  :custom
  (haskell-indentation-layout-offset 4)
  (haskell-indentation-left-offset 4)
  ;; Избегаем конфликта со сторонними форматтерами (используем ormolu ниже).
  (haskell-stylish-on-save nil)
  :bind
  (:map haskell-mode-map
        ("C-c C-l" . haskell-process-load-file)
        ("C-c C-t" . haskell-mode-show-type-at)
        ("C-c C-i" . haskell-process-do-info)))

;;;; 2. REPL и интерактивность
;; Haskell — язык с REPL-культурой, поэтому interactive-haskell-mode связывает
;; буфер с GHCi/Cabal. Это позволяет оценивать выражения на лету, логировать сессии
;; и авто-импортировать модули, мотивируя экспериментальный стиль разработки.

(use-package haskell-interactive-mode
  :defer t
  :requires haskell-mode
  :custom
  (haskell-process-type 'cabal-repl)  ; Использовать Cabal для проектов (альтернатива: ghci).
  (haskell-process-auto-import-loaded-modules t)  ; Авто-импорт модулей при загрузке.
  (haskell-process-log t)  ; Логирование всех взаимодействий для отладки.
  :bind
  (:map haskell-mode-map
        ("C-c C-z" . haskell-interactive-switch)  ; Переключение в REPL-буфер.
        ("C-c C-k" . haskell-interactive-mode-clear))  ; Очистка REPL.
  :config
  ;; Поддержка Nix: если есть flake.nix — запускаем REPL через nix develop;
  ;; если есть shell.nix — через nix-shell.
  (let ((flake-root (locate-dominating-file default-directory "flake.nix"))
        (shell-root (locate-dominating-file default-directory "shell.nix")))
    (cond
     ((and (executable-find "nix") flake-root)
      (setq haskell-process-wrapper-function
            (lambda (args)
              (append (list "nix" "develop" flake-root "--command")
                      (list (mapconcat #'identity args " "))))))
     ((and (executable-find "nix-shell") shell-root)
      (setq haskell-process-wrapper-function
            (lambda (args)
              (append (list "nix-shell" shell-root)
                      (list "--run" (mapconcat #'identity args " ")))))))))

;;;; 3. Форматирование и стиль
;; Чистый код — ключ к поддержке Haskell. Здесь мы настраиваем форматтеры
;; (stylish-haskell или ormolu), чтобы автоматически выравнивать код при сохранении.
;; Это завершает цикл написания, связывая с REPL для проверки форматированного кода.

(use-package ormolu
  :defer t
  :ensure t
  :hook (haskell-mode . ormolu-format-on-save-mode)  ; Ormolu как альтернатива stylish.
  :bind (:map haskell-mode-map
              ("C-c f" . ormolu-format-buffer)))  ; Ручное форматирование.

;; Опционально: интеграция с format-all для универсальности.


;;;; 4. Отладка и инспекция
;; Отладка в Haskell фокусируется на типах и декларациях. Haskell-decl-scan-mode
;; сканирует импорты, а doc-mode предоставляет документацию. Логи из REPL помогают
;; анализировать ошибки, логически продолжая интерактивность из секции 2.

;; (use-package haskell-debug
;;   :defer t
;;   :ensure t
;;   :hook (haskell-mode . haskell-debug-mode))  ; Отладчик для шагового выполнения.
;; Eglot: на всякий случай поднимем/настроим и в haskell-debug-mode.
(add-hook 'haskell-debug-mode-hook #'pro/haskell-eglot-setup)
(add-hook 'haskell-debug-mode-hook #'pro/haskell-eglot-ensure)

;;;; 5. Дополнительные инструменты
;; Эти инструменты дополняют базовую настройку: decl-scan для навигации по коду,
;; doc-mode для справки, и YASnippet для шаблонов. Они мотивированы необходимостью
;; быстрого доступа к информации в функциональном языке.

(use-package haskell-doc
  :defer t
  :requires haskell-mode
  :config (haskell-doc-mode t))  ; Авто-включение, но уже в хуке выше.

;;;; 6. Кастомные функции
;; Здесь кастомные утилиты, расширяющие Haskell-mode: например, быстрая оценка
;; региона в REPL. Они мотивированы необходимостью плавной интеграции редактирования
;; и выполнения, делая Emacs более responsive.

(defun haskell-eval-region ()
  "Оценить регион в interactive Haskell и вывести результат."
  (interactive)
  (if (use-region-p)
      (haskell-process-do-try (buffer-substring (region-beginning) (region-end)))
    (message "Нет активного региона.")))

(bind-key "C-c C-e" 'haskell-eval-region haskell-mode-map)

;;;; 7. Финал
;; Завершаем модуль, предоставляя его для require в других файлах.

(provide 'про-код-на-haskell)
;;; про-код-на-haskell.el ends here
