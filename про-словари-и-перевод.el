;;; словари-и-перевод.el --- Словари и переводчики
;;; Commentary:
;; Словари и переводчики
;;; Code:

(use-package go-translate
  :ensure t
  :bind (
         ("C-c v" . gts-do-translate))
  :custom ((gts-translate-list '(("en" "ru")
                                ("ru" "en"))))
  :config
  (setq gts-default-translator
       (gts-translator
        :picker (gts-prompt-picker)
        :engines (list (gts-bing-engine) (gts-google-engine))
        :render (gts-buffer-render))))

(provide 'про-словари-и-перевод)
;;; словари-и-перевод.el ends here
