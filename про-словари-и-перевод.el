;;; про-словари-и-перевод.el --- Словари и переводчики -*- lexical-binding: t -*-
;;; Commentary:
;; Словари и переводчики
;;; Code:

(use-package go-translate
  :defer t 
  :ensure t
  :functions (gt-translator
         gt-prompt-picker
         gt-bing-engine
         gt-buffer-render
         gt-google-engine
         gt-taker)
  :defines (gt-default-translator)
  :custom ((gt-translate-list '(("en" "ru")
                               ("ru" "en"))))
  :config
  (setq gt-default-translator
       (gt-translator
        :taker (gt-taker
                :langs '(en ru))
        :engines (list (gt-google-engine) (gt-bing-engine))
        :render (gt-buffer-render)))

  ;; (setq gts-default-translator (gts-translator
  ;;                              :picker (gts-prompt-picker)
  ;;                              :engines (list (gts-bing-engine)
  ;;                                             (gts-google-engine))
  ;;                              :render (gts-buffer-render)))
  )

(provide 'про-словари-и-перевод)
;;; про-словари-и-перевод.el ends here
