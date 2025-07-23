;;; про-web.el --- Базовый seed для Web (html/css/js/ts) -*- lexical-binding: t -*-
;;; Commentary:
;; Web-mode, emmet-mode, js-mode (minimal), активируются если присутствуют соответствующие use-package.
;;; Code:

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'" . web-mode))

(use-package emmet-mode
  :ensure t
  :hook ((web-mode css-mode) . emmet-mode))

(use-package js
  :ensure nil
  :mode ("\\.js\\'" . js-mode))

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" . typescript-mode))

(message "Web seed активирован.")

(provide 'про-web)
;;; про-web.el ends here
