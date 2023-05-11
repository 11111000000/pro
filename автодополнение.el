;;; автодополнение.el --- Автодополнение строк
;;; Commentary:
;; Автодополнение текста на базе Corfu
;;; Code:
;;;; Инициализация Corfu

(defun в-минибуфере-включать-corfu ()
  "Включать Corfu в минибуфере если Vertico/Mct не активны."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(use-package corfu
  :bind (:map corfu-map
              ("<escape>". corfu-quit)
              ("<return>" . corfu-insert)
              ("C-h" . corfu-info-documentation)
              ("M-l" . corfu-info-location)
              ("C-n" . corfu-quit)
              ("C-p" . corfu-quit)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto nil)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.25)
  (corfu-popupinfo-delay 0.7)
  (corfu-min-width 5)
  (corfu-max-width 30)
  (corfu-count 14)
  (corfu-scroll-margin 3)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect 'first)

  :init

  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1))

;;;; Расширения для автодополнения

(use-package cape
  :ensure t
  :init)

;;;; Автодополнение для терминала

(use-package corfu-terminal
  :ensure t)

;;;; Иконки для автодополнения

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  ;; (kind-icon-blend-background nil)
  ;; (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'kb/themes-hooks #'(lambda ()
               (interactive)
               (kind-icon-reset-cache))))

;; (defun sorting (list)
;;       (interactive))

;; (use-package codeium
;;   :init
;;   (установить-из :repo "Exafunction/codeium.el")
;;   (add-to-list 'completion-at-point-functions #'codeium-completion-at-point)
;;   :config

;;   (setq use-dialog-box nil)

;;   (setq codeium/metadata/api_key "212600da-b787-4d45-91f0-5e9e98b94302")

;;   ;; use M-x codeium-diagnose to see apis/fields that would be sent to the local language server
;;   (setq codeium-api-enabled
;;          (lambda (api)
;;            (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
;;   )

;; (use-package codeium-diagnose)
;;   :init
;;   (установить-из :repo "Exafunction/codeium-diagnose.el")
;;   :config
;;   (setq use-dialog-box nil)


(provide 'автодополнение)
;;; автодополнение.el ends here
