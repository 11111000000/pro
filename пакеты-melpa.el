;;;; Источник пакетов Melpa

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(unless (bound-and-true-p package--initialized) 
  (package-initialize))

(unless (file-exists-p "~/.emacs.d/elpa/archives/melpa") 
  (package-refresh-contents))

;;;; Макрос Use-package

(unless (package-installed-p 'use-package) 
  (package-refresh-contents) 
  (package-install 'use-package))

(eval-when-compile (progn 
                     (require 'use-package) 
                     (setq-default use-package-verbose t)))

;;; Настройки customize

;; (setq-default custom-file (expand-file-name "custom.el" user-emacs-directory))
