;;; про-оптимизацию.el --- Оптимизация загрузки и работы Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: optimization, startup, performance, gc
;; URL: https://github.com/username/emacs.d/blob/main/инфраструктура/про-оптимизацию.el
;;
;;; Commentary:
;;
;; Этот файл оптимизирует запуск и работу Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Emacs известен своей медленной загрузкой из-за
;; интенсивной работы с файлами и множеством пакетов. Здесь мы устраняем
;; узкие места: отключаем сборку мусора на время старта, откладываем
;; компиляцию, оптимизируем чтение файлов. Это даёт мгновенный запуск
;; даже при большом количестве пакетов.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Оптимизация сборки мусора
;;  2. Отложенная компиляция
;;  3. Оптимизация чтения файлов
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается автоматически через init.el как можно раньше,
;; до загрузки других модулей.  Рекомендуется подключать первым в очереди.
;;
;;; Code:
;;;; Отложим компиляцию

;; (defvar comp-deferred-compliation)
;; (setq comp-deferred-compilation t)

;;;; Временно выключим сборку мусора...

(defvar saved/gc-cons-threshold (* 64 1024 1024))
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ...и включим её обратно, когда загрузимся

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold saved/gc-cons-threshold
                   gc-cons-percentage 0.1)))

;;;; Оптимизируем использование памяти в минибуфере

(defun задержать-сборку-мусора ()
  "Задержать сборку мусора."
  (setq gc-cons-threshold most-positive-fixnum))

(defun восстановить-сборку-мусора ()
  "Восстановить сборку мусора."
  (run-at-time 1 nil
               (lambda ()
                 ;; Оптимизация: откладываем возврат GC, чтобы не мешать вводу.
                  (setq gc-cons-threshold saved/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'задержать-сборку-мусора)
(add-hook 'minibuffer-exit-hook #'восстановить-сборку-мусора)

;;;; Показывать ли сообщение о сборке мусора

(setq garbage-collection-messages nil)

;;;; Временно выключим специальную обработку файлов

(defvar saved/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist saved/file-name-handler-alist)))

;;;; Выключим файл site-run

(setq site-run-file nil)

;;;; Ускорим кэш шрифтов

(setq inhibit-compacting-font-caches t)

;;;; Ускорим ввод-вывод

(when (boundp 'read-process-output-max)
  (setq read-process-output-max (* 1024 1024))
  (setq idle-update-delay 0.02))

;;;; Двунаправленный текст

(setq-default bidi-display-reordering nil
              bidi-paragraph-direction 'left-to-right)

;;;; Более бережный скроллинг и фонтификация

(setq fast-but-imprecise-scrolling nil
      jit-lock-defer-time nil
      jit-lock-stealth-time nil ;; Оптимизация: без stealth-проходов фонтификации.
      redisplay-skip-fontification-on-input nil)

(provide 'про-оптимизацию)
;;; про-оптимизацию.el ends here
