;;; про-инструменты.el --- Разные полезные инструменты -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'use-package)
(require 'установить-из)

;;;; Открыть файл с помощью чего-нибудь

(use-package openwith
  :defer t 
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
