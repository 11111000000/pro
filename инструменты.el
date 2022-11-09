;; * Инструменты

;; ** Открыть с помощью...

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

;; ** Запускалка приложений

;; (use-package bon-app-launcher  
;;   :commands (bon-app-launcher bon-app-launcher-usr-bin)
;;   )

(use-package app-launcher  
  :straight '(app-launcher :host github :repo "SebastienWae/app-launcher")
  :bind (("s-x" . app-launcher-run-app))
  )


(provide 'инструменты)
