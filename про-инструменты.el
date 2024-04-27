;;; про-инструменты.el --- Разные полезные инструменты
;;; Commentary:
;;; Code:

(require 'use-package)

;;;; Открыть файл с помощью чего-нибудь

(use-package openwith
  :ensure t
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
  ;:ensure t
  :init (установить-из :repo "SebastienWae/app-launcher")
  :bind (("s-x" . app-launcher-run-app)))

;;;; Функции переферии

(defun выключить-микрофон () "Выключить микрофон." (interactive) (async-shell-command "amixer set Capture toggle" nil nil))
(defun выключить-звук () "Выключить звук." (interactive) (async-shell-command "pamixer -t" nil nil))
(defun увеличить-громкость () "." (interactive) (async-shell-command "pamixer -i 10" nil nil))
(defun уменьшить-громкость () "." (interactive) (async-shell-command "pamixer -d 10" nil nil))
(defun увеличить-яркость () "." (interactive) (async-shell-command "echo ok" nil nil))
(defun уменьшить-яркость () "." (interactive) (async-shell-command "echo ok" nil nil))
(defun переключить-тачпад () "." (interactive) (async-shell-command "xinput toggle 13" nil nil))


;;;; REST Client

(use-package restclient
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

(use-package
  embark
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
(use-package
  embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :after consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))


(provide 'про-инструменты)
;;; про-инструменты.el ends here
