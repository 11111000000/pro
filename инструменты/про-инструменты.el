;;; про-инструменты.el --- Разные полезные инструменты -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: tools, openwith, app-launcher, restclient
;; URL: https://github.com/username/emacs.d/blob/main/инструменты/про-инструменты.el
;;
;;; Commentary:
;;
;; Этот файл содержит разные полезные инструменты для Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Помимо основной функциональности, полезно иметь
;; инструменты для открытия файлов во внешних приложениях, запуска программ
;; из меню, HTTP-запросов через restclient. Это расширяет возможности Emacs.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Открытие файлов во внешних приложениях (openwith)
;;  2. Запуск приложений (app-launcher)
;;  3. HTTP-клиент (restclient)
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-инструменты) в init.el.
;;
;;; Code:

(require 'use-package)
(require 'установить-из)

;;;; Открыть файл с помощью чего-нибудь

(use-package openwith
  :ensure t
  :defer t
  :defines (openwith-associations)
  :functions (openwith-mode)
  :config
  (openwith-mode t)
  (setq openwith-associations '(("\\.pdf\\'" "xdg-open" (file))
                               ("\\.xlsx\\'" "soffice" (file))
                               ("\\.docx\\'" "soffice" (file))
                               ("\\.pptx\\'" "soffice" (file))
                               ("\\.csv\\'" "soffice" (file))
                               ("\\.ods\\'" "soffice" (file))
                               ("\\.xopp\\'" "xournalpp" (file)))))

;;;; Запускалка приложений из системного меню

(use-package app-launcher
  :defer t 
                                        ;:ensure t
  :init (установить-из :repo "SebastienWae/app-launcher")
  :bind (("s-x" . app-launcher-run-app)))

;;;; REST Client

(use-package restclient
  :defer t 
  :ensure t
  :hook  
  (restclient-mode . display-line-numbers-mode)
  :mode ((rx ".http" eos) . restclient-mode))

;;; Мониторинг нагрузки

;; (use-package explain-pause-mode
;;   :init (установить-из :repo "lastquestion/explain-pause-mode")
;;   :config
;;   (explain-pause-mode -1))

;;; Меню действий

(use-package embark
  :defer t
  :ensure t
  :functions (embark-prefix-help-command)
  :bind
  (("C-." . embark-act) ;; pick some comfortable binding
   ;; ("C-;" . embark-dwim) ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list
   'display-buffer-alist
   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
     nil
     (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.

(use-package embark-consult
  :defer t  
  :ensure t ; only need to install it, embark loads it after consult if found
  :after consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'про-инструменты)
;;; про-инструменты.el ends here
