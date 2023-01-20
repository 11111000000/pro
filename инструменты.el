;;; инструменты.el --- Разные полезные инструменты
;;; Commentary:
;;; Code:
;;;; Открыть файл с помощью чего-нибудь

(use-package openwith
  :ensure t
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
  :init (установить-из-репы :repo "SebastienWae/app-launcher")
  :bind (("s-x" . app-launcher-run-app)))

(provide 'инструменты)
;;; инструменты.el ends here
