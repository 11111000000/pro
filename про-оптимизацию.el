;;; оптимизация.el --- Оптимизация загрузки и работы EMACS
;;; Commentary:
;;; Code:
;;;; Отложим компиляцию

;; (defvar comp-deferred-compliation)
;; (setq comp-deferred-compilation t)

;;;; Временно выключим сборку мусора...

(defvar saved/gc-cons-threshold 800000)
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
  (setq read-process-output-max (* 1024 1024)))

(provide 'про-оптимизацию)
;;; оптимизация.el ends here
