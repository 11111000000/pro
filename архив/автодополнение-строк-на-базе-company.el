;;; Автодополнение (Company)
;;; Company

(leaf company 
  :ensure t 
  :custom ((company-idle-delay 
            1) 
           (company-prefer-capf t) 
           (company-tooltip-limit 12) 
           (company-minimum-prefix-length 3) 
           (company-echo-delay 0) 
           (company-require-match nil) 
           (company-dabbrev-downcase nil) 
           (company-tooltip-align-annotations t) 
           (company-frontends '(company-pseudo-tooltip-frontend)) 
           (company-tooltip-maximum-width 120) 
           (company-tooltip-minimum-width 20) 
           (company-tooltip-align-annotations t) 
           (company-auto-complete nil) 
           (company-global-modes '(not term-mode))
                                        ;(company-transformers '(company-sort-by-occurrence))
           (company-selection-wrap-around nil) 
           (company-show-numbers nil) 
           (company-backends 
            '((company-capf 
               company-files
               ;; company-dabbrev
               ;; company-dabbrev-code
               company-yasnippet
               ;; company-elisp
               ))))
  :bind (:map company-mode-map
              ("<tab>" . company-indent-or-complete-common) 
              ("S-<tab>" . company-complete-common) 
              ("C-c <tab>" . company-yasnippet) 
              :map org-mode-map 
              ("<tab>" . org-cycle) 
              :map company-active-map
              ("C-p" . company-select-previous) 
              ("C-n" . company-select-next) 
              ("RET" . nil)
              ;; ("C-c C-y" . my-company-yasnippet)
              :map company-search-map
              ("C-p" . company-select-previous) 
              ("C-n" . company-select-next)) 
  :config
  (global-company-mode)
  :init)

;; Быстрая справка в попапе при дополнении

;; (leaf company-quickhelp
;;   :ensure t
;;   :config
;;   (company-quickhelp-mode -1)
;;   )

;; Иконки и быстрая справка (пока непонятно, нужно ли это)

;; (leaf company-box
;;   :ensure t
;;   :hook (company-mode . nil)
;;   :config
;;   (setq company-box-backends-colors t)
;;   (setq company-box-show-single-candidate t)
;;   (setq company-box-max-candidates 12)
;;   )

;; Дополнение комманд и разметки org-mode

(defun orgos-pcomplete-capf () 
  (add-hook 'completion-at-point-functions 'pcomplete-completions-at-point nil t))

(add-hook 'org-mode-hook #'orgos-pcomplete-capf)

;; Показывать автодополнение в отдельном фрейме поверх основного окна - это позволяет использовать режимы с переменной шириной шрифта и масштабировать размеры

(leaf company-posframe 
  :ensure t 
  :custom ((company-posframe-quickhelp-delay 
            .
            nil) 
           (company-posframe-show-indicator . nil)) 
  :config (company-posframe-mode -1) 
  (require 'desktop)                    ;this line is needed.
  (push '(company-posframe-mode . nil) desktop-minor-mode-table))

;;; Сниппеты

(leaf yasnippet 
  :ensure t 
  :init (yas-global-mode 1))

(leaf yasnippet-snippets 
  :ensure t 
  :init)

;; В emacs lisp дополнять только что набранные символы

;; (defun config-company-elisp-setup ()
;;   (set (make-local-variable 'company-backends)
;;        '((company-capf :with company-dabbrev-code))))
;; (dolist (hook '(emacs-lisp-mode-hook lisp-interaction-mode-hook))
;;   (add-hook hook config-company-elisp-setup))


;; (leaf company-tabnine
;;   :ensure t
;;   :bind (("M-<tab>" . company-tabnine))
;;   :config
;; ;;(add-to-list 'company-backends #'company-tabnine)
;; )

(provide 'dobro-code-completion-company)
