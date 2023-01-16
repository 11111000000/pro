;;; словари-и-перевод.el --- Словари и переводчики
;;; Commentary:
;; Словари и переводчики
;;; Code:

(use-package go-translate
  :ensure t
  :custom (
      (gts-translate-list '(("en" "ru"))))
  :config
  (setq gts-default-translator
        (gts-translator
         :picker (gts-prompt-picker)
         :engines (list (gts-bing-engine) (gts-google-engine))
         :render (gts-buffer-render))))

(provide 'словари-и-перевод)
;;; словари-и-перевод.el ends here
