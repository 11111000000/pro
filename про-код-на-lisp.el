;;; про-код-на-lisp.el --- Поддержка Lisp в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.2
;; Keywords: lisp, elisp, geiser, macrostep, formatting
;; URL: https://example.com/про-код-на-lisp
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку Lisp в Emacs, следуя принципам литературного
;; программирования: код представлен как повествование, где каждая
;; секция мотивируется, объясняется и логически связывается с остальными. Мы
;; стремимся к элегантности, минимализму и производительности в лучших традициях
;; Emacs — с использованием `use-package` для декларативной конфигурации, хуков
;; для автоматизации и отложенной загрузки для скорости.
;;
;; Почему это важно? Lisp — это язык Emacs, и эффективная работа с ним усиливает
;; всю среду. Здесь мы фокусируемся на Emacs Lisp как основном диалекте, добавляя
;; инструменты для отладки, форматирования и REPL для других Lisp-ов, делая Emacs
;; мощным IDE без излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require и утилиты.
;; 1. Базовые функции: Выполнение кода в регионах и буферах.
;; 2. Настройки Emacs Lisp: Биндинги, печать и оптимизации.
;; 3. Отладка и инспекция: Инструменты для поиска ошибок и анализа объектов.
;; 4. Форматирование: Автоматическое выравнивание кода.
;; 5. REPL и диалекты: Geiser для разных Lisp-ов.
;; 6. Макросы и расширения: Разворачивание и оверлеи результатов.
;; 7. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-код-на-lisp) в вашем init.el.
;; Рекомендуется интегрировать с Eglot для LSP (если нужно). Закомментированные
;; секции — опции для экспериментов (например, альтернативные форматтеры).
;;
;; Замечания: Мы предпочитаем отложенную загрузку (:defer t), локальные бинды
;; и минимальные глобальные изменения. Для конфликтов — проверяйте remap.

;;; Code:

;;;; 0. Введение и зависимости
;; Здесь мы подключаем базовые пакеты. `use-package` обеспечивает модульность,
;; а `установить-из` — удобную установку из репозиториев. Это основа: без них
;; дальнейшие секции не смогут работать декларативно.

(require 'use-package)
(require 'установить-из)

;;;; 1. Базовые функции
;; Начинаем с сущностного: выполнения кода. Lisp — это REPL-ориентированный язык,
;; поэтому удобное выполнение регионов или буферов — ключ к продуктивности. Мы
;; определяем функции, которые связывают интерактивные команды с eval, учитывая
;; регион и деактивацию метки.

(defun выполнить-регион-или-буфер ()
  "Выполнить регион (если активен) или весь буфер.
Это упрощает отладку: быстрый eval без копирования в scratch."
  (interactive)
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (перевыполнить-буфер)))

(defun перевыполнить-буфер ()
  "Перевыполнить все формы в буфере с помощью `eval-defun'.
Верхнеуровневые формы оцениваются заново, включая `defvar' и `defcustom',
что полезно для конфигурационных файлов Emacs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (forward-sexp)
      (eval-defun nil))))

;;;; 2. Настройки Emacs Lisp
;; Emacs Lisp — диалект для расширения Emacs. Здесь мы настраиваем биндинги для
;; выполнения, параметры печати выражений и оптимизации. Это связывает базовые
;; функции с режимом, делая работу с elisp естественной.

(use-package emacs
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . выполнить-регион-или-буфер)
              ("C-x M-e" . eval-print-last-sexp)))

;;;;; 2.1 Печать выражений
;; Увеличиваем лимиты для eval-expression-print, чтобы вывод был полным в
;; сложных структурах данных — это помогает в отладке без усечения.
(setq eval-expression-print-level 15)
(setq eval-expression-print-length 30)

;;;;; 2.2 Оптимизация хвостовой рекурсии
;; Tco преобразует хвостовую рекурсию в циклы, избегая переполнения стека —
;; критично для функционального стиля в Lisp.
(use-package tco
  :defer t
  :ensure t)

;;;;; 2.3 Асинхронные функции
;; Async-await добавляет промисы и await, упрощая асинхронный код в Emacs Lisp —
;; полезно для пакетов с сетевыми запросами.
(use-package async-await
  :defer t
  :ensure t)

;;;; 3. Отладка и инспекция
;; Отладка — это искусство: здесь мы добавляем инструменты для поиска багов,
;; инспекции объектов и проверки синтаксиса. Это логическое продолжение секции 1,
;; где выполнение может выявить ошибки, требующие анализа.

;;;;; 3.1 Автоматическое тестирование конфига
;; Bug-hunter тестирует init.el на ошибки, запуская секции по порядку —
;; неоценимо для больших конфигураций, но отключено по умолчанию для скорости.
(use-package bug-hunter
  :defer t
  :ensure t)

;;;;; 3.2 Инспектор объектов
;; Inspector предоставляет интерактивный просмотр Lisp-объектов, как в REPL —
;; связывает с печатью выражений из секции 2.
(use-package inspector
  :defer t
  :ensure t)

;;;;; 3.3 Проверка синтаксиса
;; Flymake-elisp-config добавляет flymake для elisp, проверяя синтаксис на лету —
;; интегрируется с Eglot для полной LSP-поддержки.
(use-package fly	make-elisp-config
  :defer t
  :init (установить-из :repo "ROCKTAKEY/flymake-elisp-config")
  :functions (flymake-elisp-config-global-mode flymake-elisp-config-auto-mode)
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))

;;;; 4. Форматирование
;; Чистый код — читаемый код. Здесь мы настраиваем форматтеры для elisp,
;; чтобы автоматически выравнивать отступы и стиль — это завершает цикл
;; написания и выполнения из предыдущих секций.

(use-package elisp-format
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . (lambda ()
                             (add-hook 'before-save-hook #'elisp-format-buffer nil t))))

(use-package format-all
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . format-all-mode))

;;;; 5. REPL и диалекты
;; REPL — сердце Lisp. Geiser предоставляет унифицированный REPL для разных
;; имплементаций (Guile, Racket и т.д.), расширяя Emacs за пределы elisp.

(use-package geiser
  :defer t
  :ensure t
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile)))
  (geiser-mode-start-repl-p nil))

(use-package geiser-guile
  :defer t
  :ensure t
  :requires geiser
  :config
  (setq geiser-guile-manual-lookup-nodes '("guile" "guix")))

;;;; 6. Макросы и расширения
;; Макросы — мощь Lisp. Здесь инструменты для разворачивания и визуализации,
;; плюс оверлеи результатов eval — это углубляет отладку из секции 3.

(use-package macrostep
  :defer t
  :ensure t
  :custom-face
  (macrostep-expansion-highlight-face
   ((t (:inherit default :extend t :background ,(face-attribute 'default :background)))))
  :bind (:map emacs-lisp-mode-map
              ("C-c >" . macrostep-expand)
              ("C-c <" . macrostep-collapse)
              :map lisp-interaction-mode-map
              ("C-c >" . macrostep-expand)
              ("C-c <" . macrostep-collapse)))

(use-package eros
  :defer t
  :ensure t
  :functions (eros-mode)
  :config
  (eros-mode t))

;;;; 7. Финал
;; Завершаем модуль, предоставляя его для require в других файлах.

(provide 'про-код-на-lisp)
;;; про-код-на-lisp.el ends here
