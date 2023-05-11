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


(provide 'инструменты)
;;; инструменты.el ends here
