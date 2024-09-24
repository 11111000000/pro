;;; про-интернет-общение.el --- общение в интернет -*- lexical-binding: t -*-
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

(defun выбрать-контакт-в-телеге ()
  "Выбор из всех контактов и чатов Telega с помощью consult."
  (interactive)
  (require 'telega)
  (require 'consult)
  (let* ((contacts (telega--getContacts))
        (formatted-contacts (mapcar (lambda (user)
                                     (cons (telega--tl-get user :first_name) user))
                                   contacts)))
    (consult--read (mapcar 'car formatted-contacts)
                   :prompt "Выберите контакт или чат: "
                   :category 'telega-contact-chat
                   :require-match t
                   :history 'consult--telega-contacts-chats-history
                   :sort t
                   :state (lambda (action selected)
                            (when selected
                              (let ((choice (cdr (assoc selected formatted-contacts))))
                                (telega-chat-with choice)))))))

(provide 'про-интернет-общение)
;;; про-интернет-общение.el ends here
