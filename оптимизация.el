;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Ускорение и оптимизации

;; Отложим компиляцию

;; (defvar comp-deferred-compliation)
;; (setq comp-deferred-compilation t)

;; Временно выключим сборку мусора...

(defvar dobro/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; ...и включим её обратно, когда загрузимся

(add-hook 'emacs-startup-hook 
          (lambda () 
            (setq gc-cons-threshold dobro/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; Оптимизируем использование памяти в минибуфере

(defun dobro/defer-garbage-collection-h () 
  (setq gc-cons-threshold most-positive-fixnum))

(defun dobro/restore-garbage-collection-h ()
  (run-at-time 1 nil 
               (lambda () 
                 (setq gc-cons-threshold dobro/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'dobro/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'dobro/restore-garbage-collection-h)

;; Показывать сообщение о сборке мусора

;; (setq garbage-collection-messages t)

;; Временно выключим специальную обработку файлов

(defvar dobro/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook 
          (lambda () 
            (setq file-name-handler-alist dobro/file-name-handler-alist)))

;; Выключим файл site-run

(setq site-run-file nil)

;; Ускорим кэш шрифтов

(setq inhibit-compacting-font-caches t)

;; Ускорим ввод-вывод

(when (boundp 'read-process-output-max) 
  (setq read-process-output-max (* 1024 1024)))

;; Файл со внешними настройками

(setq custom-file "~/.emacs.d/custom.el")

;; Но не загружаем его

;;(ignore-errors (load custom-file)) 


(provide 'оптимизация)
