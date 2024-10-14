;;; шрифты.el --- Шрифты настройки -*- lexical-binding: t -*-
;;; Commentary:
;; Этот файл настраивает шрифты в Emacs для различных режимов.
;; Включает режим смешанных шрифтов для более эстетичного отображения текста.
;;; Code:

;; Подключение пакета для смешанных шрифтов (mixing pitch)
(use-package mixed-pitch
  :ensure t  ;; Убедитесь, что пакет будет установлен
  :hook
  ;; Включаем режим смешанных шрифтов в org-mode и help-mode
  (org-mode . mixed-pitch-mode)  ;; В org-режиме используем смешанные шрифты
  (help-mode . mixed-pitch-mode)  ;; В режиме справки также используем смешанные шрифты
  :custom
  (mixed-pitch-variable-pitch-cursor nil)  ;; Отключаем курсор переменной ширины
  :config
  (setq
   mixed-pitch-fixed-pitch-faces
   (delete-dups  ;; Удаляем дублирующиеся элементы
    (append
     mixed-pitch-fixed-pitch-faces  ;; Существующие лица фиксированной ширины
     '(font-lock-comment-delimiter-face font-lock-comment-face org-block
                                        org-block-begin-line org-block-end-line org-cite org-cite-key
                                        org-document-info-keyword org-done org-drawer org-footnote org-formula
                                        org-inline-src-block org-latex-and-related org-link org-code org-column
                                        org-column-title org-date org-macro org-meta-line org-property-value
                                        org-quote org-ref-cite-face org-sexp-date org-special-keyword org-src
                                        org-table org-tag org-tag-group org-todo org-verbatim org-verse)))))


;; Установить шрифты по умолчанию в новом фрейме.
;; (setq-default default-frame-alist '((font . "Fira Code")))

;; Функция для обновления настроек шрифтов
(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  ;; Закомментированная часть позволяет использовать различные шрифты:
  ;; (custom-set-faces '(default ((t (:family "DejaVu Sans Mono" :height 120))))) ;; Для стандартного текста
  ;; (custom-set-faces '(fixed-pitch ((t (:family "DejaVu Sans Mono" :height 0.8))))) ;; Для фиксированного текста
  ;; (custom-set-faces '(variable-pitch ((t (:family "DejaVu Sans" :height 1.0))))) ;; Для переменной ширины текста
  (custom-set-faces '(default ((t (:family "Source Code Pro" :height 120))))) ;; Настраиваем шрифт по умолчанию
  (custom-set-faces '(fixed-pitch ((t (:family "Source Code Pro" :height 0.8))))) ;; Настраиваем фиксированный шрифт
  (custom-set-faces '(variable-pitch ((t (:family "Source Serif Pro" :height 1.0)))))) ;; Настраиваем переменную ширину

;; Добавление поддержки для эмодзи в шрифтовом наборе
(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

;; Обновляем настройки шрифтов, когда загружается новая тема
(add-hook 'after-load-theme-hook
         (lambda ()
           (обновить-настройки-шрифтов)))

;; Инициализация настроек шрифтов
(обновить-настройки-шрифтов)

;;;; Межстрочный интервал
(setq-default line-spacing 1)  ;; Устанавливаем межстрочный интервал на единицу для улучшения читаемости

;;;; Красивые типографические символы
;; Заменяем последовательности символов на красивые глифы
;; При наведении курсора хотим видеть оригинал:
(use-package fira-code-mode
  :if window-system  ;; Убедимся, что это только для графических систем
  :ensure t
  :hook ((prog-mode . fira-code-mode))  ;; Включаем fira-code-mode в режимах программирования
  :functions (fira-code-mode-set-font)  ;; Указываем, что будем использовать эту функцию
  :config
  (fira-code-mode-set-font))  ;; Настраиваем шрифт Fira Code для использования

;; Включаем "красивые" символы в тексте
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point t)  ;; Возвращаем оригинальный символ при наведении курсора

(provide 'про-шрифты) ;; Экспортируем текущий пакет
;;; про-шрифты.el ends here
