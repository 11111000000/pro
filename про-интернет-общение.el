;;; про-интернет-общение.el --- общение в интернет
;;; Commentary:
;; Конфигурация мессенджеров
;;; Code:

;;;; Telegram

(require 'установить-из)

(use-package telega
  :init (установить-из :repo "zevlg/telega.el")
  :custom ((telega-use-docker t)
          (telega-use-images t)
          (telega-chat-fill-column 80)
          ;; (telega-emoji-use-images t)
          (telega-emoji-font-family "Noto Color Emoji"))
  :hook ((telega-root-mode . telega-notifications-mode)
       (telega-load-hook . global-telega-url-shorten-mode)
       (telega-root-mode . hl-line-mode)
       (telega-chat-mode . variable-pitch-mode))
  :config)

(provide 'про-интернет-общение)
;;; интернет-общение.el ends here
