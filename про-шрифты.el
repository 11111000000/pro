;;; шрифты.el --- Шрифты -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mixed-pitch
  :ensure t
  :hook
  (org-mode . mixed-pitch-mode)
  (help-mode . mixed-pitch-mode)
  :custom
  (mixed-pitch-variable-pitch-cursor nil)
  :config
  (setq
   mixed-pitch-fixed-pitch-faces
   (delete-dups
    (append
     mixed-pitch-fixed-pitch-faces
     '(font-lock-comment-delimiter-face font-lock-comment-face org-block
                                        org-block-begin-line org-block-end-line org-cite org-cite-key
                                        org-document-info-keyword org-done org-drawer org-footnote org-formula
                                        org-inline-src-block org-latex-and-related org-link org-code org-column
                                        org-column-title org-date org-macro org-meta-line org-property-value
                                        org-quote org-ref-cite-face org-sexp-date org-special-keyword org-src
                                        org-table org-tag org-tag-group org-todo org-verbatim org-verse)))))


;;(setq-default default-frame-alist '((font . "Fira Code")))

;; function to convert from rubles to usd

(defun обновить-настройки-шрифтов ()
  "Настройки шрифтов."
  (interactive)
  ;; (custom-set-faces '(default ((t (:family "DejaVu Sans Mono" :height 120)))))
  ;; (custom-set-faces '(fixed-pitch ((t (:family "DejaVu Sans Mono" :height 0.8)))))
  ;; (custom-set-faces '(variable-pitch ((t (:family "DejaVu Sans" :height 1.0)))))
  (custom-set-faces '(default ((t (:family "Source Code Pro" :height 120)))))
  (custom-set-faces '(fixed-pitch ((t (:family "Source Code Pro" :height 0.8)))))
  (custom-set-faces '(variable-pitch ((t (:family "Source Serif Pro" :height 1.0))))))

(set-fontset-font "fontset-default" 'unicode "Noto Emoji" nil 'prepend)

(add-hook 'after-load-theme-hook
          (lambda ()
              (обновить-настройки-шрифтов)))

(обновить-настройки-шрифтов)

;; Межстрочный интервал

(setq-default line-spacing 1)

(provide 'про-шрифты)
;;; про-шрифты.el ends here
