;;; дополнение-строк.el --- Дополнение строк
;;; Commentary:
;; Автодополнение текста на базе Corfu
;;; Code:
;;;; Инициализация Corfu

;; https://github.com/minad/corfu#completing-with-corfu-in-the-minibuffer
(defun в-минибуфере-включать-corfu ()
  "Включать Corfu в минибуфере если Vertico/Mct не активны."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (setq-local corfu-auto nil)
    (corfu-mode 1)))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ("<escape>". corfu-quit)
              ("<return>" . corfu-insert)
              ("C-h" . corfu-info-documentation)
              ("M-l" . corfu-info-location)
              ("C-n" . corfu-quit)
              ("C-p" . corfu-quit)
              ("M-n" . corfu-next)
              ("M-p" . corfu-previous)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-auto-prefix 3)
  (corfu-auto-delay 1)
  ;; (corfu-min-width 80)
  ;; (corfu-max-width corfu-min-width)
  (corfu-count 5)
  (corfu-scroll-margin 2)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current 'insert)
  (corfu-preselect-first t)

  :init

  (global-corfu-mode)
  (add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1))

;;;; Расширения для автодополнения

(use-package cape
  :ensure t
  :bind (("C-c o p" . completion-at-point) ;; capf
         ("C-c o t" . complete-tag)        ;; etags
         ("C-c o d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c o h" . cape-history)
         ("C-c o f" . cape-file)
         ("C-c o k" . cape-keyword)
         ("C-c o s" . cape-symbol)
         ("C-c o a" . cape-abbrev)
         ("C-c o i" . cape-ispell)
         ("C-c o l" . cape-line)
         ("C-c o w" . cape-dict)
         ("C-c o \\" . cape-tex)
         ("C-c o _" . cape-tex)
         ("C-c o ^" . cape-tex)
         ("C-c o &" . cape-sgml)
         ("C-c o r" . cape-rfc1345))
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
  ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
)

;;;; Документация для автодополнения

(use-package corfu-doc
  :ensure t
  :after corfu
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-?" . #'corfu-doc-toggle)
              ("C-v" . #'corfu-doc-scroll-up)
              ("M-v" . #'corfu-doc-scroll-down))
  :custom

  (corfu-doc-delay .8)
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  (corfu-echo-documentation nil))

;;;; Автодополнение для терминала

(use-package corfu-terminal
  :ensure t)

;;;; Иконки для автодополнения

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  (kind-icon-blend-background nil)
  (kind-icon-blend-frac 0.08)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (add-hook 'kb/themes-hooks #'(lambda ()
               (interactive)
               (kind-icon-reset-cache))))

;;;; История автодополнения

;; (use-package corfu-history
;;   :after corfu
;;   :config
;;   (with-eval-after-load 'safehist
;;     (cl-pushnew 'corfu-history savehist-additional-variables))
;;   (corfu-history-mode))



(provide 'дополнение-строк)
;;; дополнение-строк.el ends here
