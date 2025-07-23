;;; про-clojure.el --- Минималистичный seed для Clojure -*- lexical-binding: t -*-
;;; Commentary:
;; Поддержка Clojure (cider, clojure-mode) только если найден clojure или lein.
;;; Code:

(when (or (executable-find "clojure")
          (executable-find "lein"))
  (use-package clojure-mode
    :ensure t
    :mode ("\\.clj\\'" . clojure-mode))
  (use-package cider
    :ensure t
    :after clojure-mode
    :hook (clojure-mode . cider-mode))
  (message "Clojure seed активирован."))

(provide 'про-clojure)
;;; про-clojure.el ends here
