;;; интернет-общение.el --- общение в интернет
;;; Commentary:
;; Конфигурация мессенджеров
;;; Code:

;;;; Telegram

(use-package telega
  :ensure t
  :custom ((telega-use-docker t)
          (telega-use-images t)
          (telega-chat-fill-column 80)
          ;; (telega-emoji-use-images t)
          (telega-emoji-font-family "Noto Color Emoji"))
  :hook ((telega-root-mode . telega-notifications-mode)
       (telega-load-hook . global-telega-url-shorten-mode)
       (telega-root-mode . hl-line-mode))
  :config)

(provide 'интернет-общение)
;;; интернет-общение.el ends here
