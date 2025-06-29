;;; про-вкладки.el --- Вкладки -*- lexical-binding: t -*-
;; Вкладки
;;; Commentary:
;;; Code:
;;;;  Верхний уровень вкладок

;; Использование пакета powerline для оформления (TODO - вытащить только глифы)
(use-package powerline
  :ensure t)

;; Функция для замены строк. Принимает пары-для-замены и строку.
(defun заменить-строки (пары-для-замены строка)
  "заменяет набор паттернов в строке.
принимает ПАРЫ-ДЛЯ-ЗАМЕНЫ и СТРОКА"
  (seq-reduce
   (lambda (строка пара)
     (string-replace
      (car пара)
      (cdr пара)
      строка))
   пары-для-замены
   строка))

;; Требовать пакет all-the-icons для отображения значков.
(require 'all-the-icons)

;; Настройка пакета tab-bar для управления вкладками.
(use-package tab-bar
  :bind (:map tab-bar-mode-map
                ("C-S-<iso-lefttab>" . nil)
                ("C-<tab>" . nil)
                :map help-quick-use-map
                ("C-S-<iso-lefttab>" . nil)
                ("C-<tab>" . nil))
  
  :custom
  (tab-bar-new-button-show nil)  ;; Скрыть кнопку создания новой вкладки.
  (tab-bar-close-button-show nil) ;; Скрыть кнопку закрытия вкладки.
  (tab-bar-separator " ")         ;; Установить разделитель вкладок.
  (tab-bar-auto-width nil)        ;; Отключить автоматическое изменение ширины вкладок.
  :config
  ;; Определение клавиш для переключения между вкладками.
  (defun tab-bar--define-keys ()
    "Установит привязки клавиш для переключения между вкладками, если их настроил пользователь"
    (when tab-bar-select-tab-modifiers
      (global-set-key (vector (append tab-bar-select-tab-modifiers (list ?0)))
                  'tab-recent)
      (dotimes (i 8)
        (global-set-key (vector (append tab-bar-select-tab-modifiers
                                 (list (+ i 1 ?0))))
                    'tab-bar-select-tab))
      (global-set-key (vector (append tab-bar-select-tab-modifiers (list ?9)))
                  'tab-last))

    (when (and (memq 'tab-bar-format-global tab-bar-format)
            (member '(global-mode-string ("" global-mode-string))
                    mode-line-misc-info))
      (setf (alist-get 'global-mode-string mode-line-misc-info)
           '(("" (:eval (if (and tab-bar-mode
                                (memq 'tab-bar-format-global
                                      tab-bar-format))
                            "" global-mode-string)))))))

  ;; Убрать дефолтные клавиши (мешают)


  (with-eval-after-load 'tab-bar
    (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
    (define-key global-map (kbd "C-<tab>") nil)
    (define-key tab-bar-mode-map (kbd "C-<tab>") nil)
    (define-key tab-bar-mode-map (kbd "C-<iso-lefttab>") nil))
  
  (defvar tab-bar-mode-map
    (let ((map (make-sparse-keymap)))
      (tab-bar-mode--tab-key-bind map [(control tab)] nil)
      (tab-bar-mode--tab-key-bind map [(control shift tab)] nil)
      (tab-bar-mode--tab-key-bind map [(control shift iso-lefttab)] nil)
      map)
    "Tab Bar mode map.")
  
  ;; Привязка клавиш для быстрого переключения вкладок.
  (dotimes (i 10)
    (global-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i))))

  ;; Функция форматирования отображения вкладок.
  (defun формат-вкладки-tab-bar (tab i)
    (let* ((высота-вкладки 18)
          (длинна-имени-вкладки 25)
          (иконка-по-умолчанию (all-the-icons-octicon "browser" :height 1  :v-adjust 0.1))
          (иконка-firefox (all-the-icons-faicon "firefox" :height 1  :v-adjust 0))
          (иконка-chrome (all-the-icons-faicon "chrome" :height 1  :v-adjust 0))
          (иконка-telegram (all-the-icons-faicon "comment" :height 1  :v-adjust 0))
          (замены-имён-вкладки `(("Firefox-esr" . ,иконка-firefox)
                                 ("firefox-default" . ,иконка-firefox)
                                 ("Google-chrome" . ,иконка-chrome)))
          (вкладка-текущая? (eq (car tab) 'current-tab))
          (имя-буфера (substring-no-properties (alist-get 'name tab)))
          (режим-вкладки (if (bufferp (get-buffer имя-буфера))
                             (with-current-buffer имя-буфера major-mode) nil))
          (иконка-режима (all-the-icons-icon-for-mode режим-вкладки :height 0.8))
          (иконка-вкладки (if (symbolp иконка-режима) иконка-по-умолчанию иконка-режима))
          (фейс-текущей-вкладки (if вкладка-текущая? 'tab-bar-tab 'tab-bar-tab-inactive))
          (укороченое-имя  (заменить-строки замены-имён-вкладки
                                            имя-буфера))
          (имя-вкладки (format "%s" (if (> (length укороченое-имя) длинна-имени-вкладки)
                                      (concat
                                       (substring укороченое-имя 0 длинна-имени-вкладки) "…")
                                    укороченое-имя)))
          (текст-вкладки (concat
                          " "
                          иконка-вкладки
                          " "
                          имя-вкладки
                          " "
                          )))
                                        ;(pp буфер-вкладки)
                                        ;(pp иконка-режима)
      (add-face-text-property 0 (length текст-вкладки) фейс-текущей-вкладки t текст-вкладки)
      текст-вкладки))

  (setq tab-bar-tab-name-format-function  #'формат-вкладки-tab-bar)
  (setq tab-bar-tab-name-function #'tab-bar-tab-name-current)

  (tab-bar-mode t)
  (tab-bar-history-mode t))

;; Определение клавиш, которые работают в режиме tab-bar

(defun открыть-новую-вкладку ()
  "Открыть новую вкладку с дашбордом."
  (interactive)
  (tab-bar-new-tab-to)
  (dashboard-open))

;;;;  Вкладки уровня окна

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-separator "")
  (tab-line-switch-cycling t)
  (tab-line-tabs-function 'tab-line-tabs-mode-buffers)
  :hook ((vterm-mode . tab-line-mode)
       (telega-mode . tab-line-mode))
  :bind (:map tab-line-mode-map
                ("C-<tab>" . tab-line-switch-to-next-tab)
                ("C-S-<iso-lefttab>" . tab-line-switch-to-prev-tab)
                ("s-]" . tab-line-switch-to-next-tab)
                ("s-[" . tab-line-switch-to-prev-tab)
                ("s-M-n" . tab-line-switch-to-next-tab)
                ("s-M-p" . tab-line-switch-to-prev-tab))
  :config

  (with-eval-after-load 'tab-line (lambda ()
                             (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
                             (define-key global-map (kbd "C-<tab>") nil)))

  
  (defvar высота-tab-line 20)

  ;; (custom-set-faces
  ;;  '(tab-line ((t (:height 1.0 :box nil :underline nil :overline nil :strike-through nil))))
  ;;  '(tab-line-tab ((t (:inherit 'default :foreground "#555555" :height 120 :box nil ))))
  ;;  '(tab-line-tab-current ((t (:inherit 'default :height 120 :box nil))))
  ;;  '(tab-line-tab-inactive ((t (:foreground "#333333" :background "#aaaaaa" :height 1.0 :box nil))))
  ;;  '(tab-line-tab-inactive-alternate ((t (:height 1.0 :box nil :background "#888888")))))

  (require 'powerline)
  
  (defun формат-имени-вкладки-tab-line (buffer &optional _buffers)
    (powerline-render (list (powerline-wave-right 'tab-line nil (+ 1 высота-tab-line))
                            (format "%s" (buffer-name buffer))
                            (powerline-wave-left nil 'tab-line (+ 1 высота-tab-line)))))

  (setq tab-line-tab-name-function #'формат-имени-вкладки-tab-line)

  (add-hook 'after-load-theme-hook
           (lambda ()
             (powerline-reset))))

(defun закрыть-вкладку-и-буфер ()
  "Закрывает вкладку и буфер в ней."
  (interactive)
  (kill-this-buffer)
  (tab-close))

(provide 'про-вкладки)
;;; про-вкладки.el ends here
