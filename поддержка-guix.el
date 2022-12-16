;;; package --- Summary
;; Guix
;;; Commentary:
;;; Code:

;;; Пакеты

(if (file-exists-p "/home/az/.guix-profile/share/emacs/site-lisp")
    (add-to-list 'load-path "/home/az/.guix-profile/share/emacs/site-lisp")
  (guix-emacs-autoload-packages))

;;;  Geiser Guile

(use-package geiser-guile  
  :config
;; (add-to-list 'geiser-guile-load-path "~/System/channels/nonguix")
  (add-to-list 'geiser-guile-load-path "~/System/channels/chan"))

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/System/channels/guix/etc/snippets"))

(provide 'поддержка-guix)
;;; поддержка-guix.el ends here
