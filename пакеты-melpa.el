;;; пакеты-melpa.el --- Пакеты MELPA
;;; Commentary:
;;; Code:
;;;; Источник пакетов Melpa

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized) 
  (package-initialize))

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa") 
  (package-refresh-contents))

;;;; Макрос leaf

(unless (package-installed-p 'leaf) 
  (package-refresh-contents) 
  (package-install 'leaf))

(eval-when-compile (progn 
                     (require 'leaf) 
                     (setq-default leaf-verbose t)))

;;; Настройки customize
;; (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
