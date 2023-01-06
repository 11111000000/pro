;;; инструменты.el --- Разные полезные инструменты
;;; Commentary:
;;; Code:
;;;; Открыть файл с помощью чего-нибудь

(leaf openwith
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

(leaf app-launcher  
  :el-get SebastienWae/app-launcher
  :bind (("s-x" . app-launcher-run-app))
  )

;; (leaf bon-app-launcher  
;;   :commands (bon-app-launcher bon-app-launcher-usr-bin)
;;   )



(provide 'инструменты)
;;; инструменты.el ends here
