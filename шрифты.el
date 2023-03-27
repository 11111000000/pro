;;; шрифты.el --- Шрифты
;;; Commentary:
;;; Code:

;; (use-package fixed-pitch
;;   :ensure t
;;   :init (установить-из-репы :repo "cstby/fixed-pitch-mode")
;;   :config
;;   (fixed-pitch-mode))

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  (help-mode . mixed-pitch-mode)
  )

;(setq-default default-frame-alist '((font . "Fira Code")))

(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  (custom-set-faces '(default ((t (:family "Fira Code" :height 120)))))
  (custom-set-faces '(fixed-pitch ((t (:family "Fira Code" :height 1.0)))))
  (custom-set-faces '(variable-pitch ((t (:family "Fira Sans" :height 1.3)))))
  ;; (let* ((variable-tuple (cond
  ;;                        ;; ((x-list-fonts "OldSlavic")         '(:font "OldSlavic"))
  ;;                        ((x-list-fonts "Golos")         '(:font "Golos"))
  ;;                        ((x-list-fonts "DejaVu")         '(:font "DejaVu Sans"))
  ;;                        ((x-list-fonts "Scientia")         '(:font "Scientia"))
  ;;                        ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  ;;                        ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  ;;                        ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  ;;                        ((x-list-fonts "Verdana")         '(:font "Verdana"))
  ;;                        ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  ;;                        (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  ;;       (base-font-color     (face-foreground 'default nil 'default))
  ;;       (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  ;;                     '(variable-pitch ((t (:family "Golos")))))
    
    ;; (custom-theme-set-faces
    ;;  'user
    ;;  `(org-level-8 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-7 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-6 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-5 ((t (,@headline ,@variable-tuple))))
    ;;  `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1 :weight normal))))
    ;;  `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1 :weight normal))))
    ;;  `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.2 :weight normal))))
    ;;  `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.3 :weight normal))))
    ;;  `(org-document-title ((t (,@headline ,@variable-tuple :height 2.1 :underline nil :weight normal)))))
    ;;                     :family "DejaVu Sans"
    ;;                     :weight 'normal
    ;;                     :height 160)
    ;; (set-face-attribute 'fixed-pitch nil
    ;;                     :family "Fira Code"
    ;;                     :weight 'normal)
    ;; (set-face-attribute 'variable-pitch nil
    ;;                     :family "DejaVu Sans"
    ;;                     :weight 'normal)
    )

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook
         (lambda ()
           (обновить-настройки-шрифтов)))

(обновить-настройки-шрифтов)

;; Межстрочный интервал

(setq-default line-spacing 1)

(provide 'шрифты)
;;; шрифты.el ends here
