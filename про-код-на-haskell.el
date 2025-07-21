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
;; секция мотивируется, объясняется и логически связывается с остальными. Мы
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

(use-package haskell-mode
  :ensure t
  :defer t
  :hook ((haskell-mode . eglot-ensure)  ; Авто-запуск LSP для интеллектуальной помощи.
         (haskell-mode . haskell-decl-scan-mode)  ; Сканирование деклараций для импорта/навигации.
         (haskell-mode . haskell-doc-mode)  ; Документация по функциям/типам на hover.
         (haskell-mode . interactive-haskell-mode)  ; Интерактивный REPL для оценки выражений.
         ;; (haskell-mode . subword-mode)  ; Опционально: навигация по camelCase именам.
         ;; (haskell-mode . haskell-indentation-mode)  ; Опционально: альтернативная индентация.
         (haskell-mode . yas-minor-mode))  ; Сниппеты для быстрого ввода шаблонов (e.g., module).
  :custom
  (haskell-indentation-layout-offset 4)  ; Отступ 4 пробела для читаемости.
  (haskell-indentation-left-offset 4)  ; Аналогично для левого отступа.
  (haskell-stylish-on-save t)  ; Авто-форматирование при сохранении (ormolu или stylish-haskell).
  :bind
  (:map haskell-mode-map
        ("C-c C-l" . haskell-process-load-file)  ; Загрузка файла в REPL.
        ("C-c C-t" . haskell-mode-show-type-at)  ; Показ типа под курсором.
        ("C-c C-i" . haskell-process-do-info))  ; Инфо о символе в REPL.
  :config
  ;; Интеграция Eglot с Haskell Language Server (HLS) для полной LSP-поддержки.
  (add-to-list 'eglot-server-programs '(haskell-mode "haskell-language-server-wrapper" "--lsp")))

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
        ("C-c C-k" . haskell-interactive-mode-clear)))  ; Очистка REPL.

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
(use-package format-all
  :defer t
  :ensure t
  :hook (haskell-mode . format-all-mode)
  :custom (format-all-formatters '(("Haskell" ormolu))))

;;;; 4. Отладка и инспекция
;; Отладка в Haskell фокусируется на типах и декларациях. Haskell-decl-scan-mode
;; сканирует импорты, а doc-mode предоставляет документацию. Логи из REPL помогают
;; анализировать ошибки, логически продолжая интерактивность из секции 2.

(use-package haskell-debug
  :defer t
  :ensure t
  :hook (haskell-mode . haskell-debug-mode))  ; Отладчик для шагового выполнения.

;;;; 5. Дополнительные инструменты
;; Эти инструменты дополняют базовую настройку: decl-scan для навигации по коду,
;; doc-mode для справки, и YASnippet для шаблонов. Они мотивированы необходимостью
;; быстрого доступа к информации в функциональном языке.

(use-package haskell-doc
  :defer t
  :requires haskell-mode
  :config (haskell-doc-mode t))  ; Авто-включение, но уже в хуке выше.

(use-package yasnippet
  :defer t
  :ensure t
  :config (yas-reload-all))  ; Перезагрузка сниппетов для Haskell.

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
