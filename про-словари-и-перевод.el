;;; про-словари-и-перевод.el --- Словари и переводчики
;;; Commentary:
;; Словари и переводчики
;;; Code:

(use-package go-translate
  :ensure t
  :functions (gts-translator gts-prompt-picker gts-bing-engine gts-buffer-render)
  :defines (gts-default-translator)
  :custom ((gts-translate-list '(("en" "ru")
                                ("ru" "en"))))
  :config
  (setq gts-default-translator (gts-translator
                               :picker (gts-prompt-picker)
                               :engines (list (gts-bing-engine) (gts-google-engine))
                               :render (gts-buffer-render))))

(provide 'про-словари-и-перевод)
;;; про-словари-и-перевод.el ends here
