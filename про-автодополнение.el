;;; про-автодополнение.el --- Автодополнение строк -*- lexical-binding: t -*-
;;; Commentary:
;; Автодополнение текста (на базе Corfu)
;;; Code:
;;;; Инициализация Corfu

;; https://github.com/minad/corfu

(use-package corfu
  :ensure t
  :defines (corfu-map)
  :functions (corfu-mode global-corfu-mode corfu-popupinfo-mode corfu-history-mode)
  :bind (:map corfu-map
              ("<escape>" . corfu-quit)
              ("<return>" . corfu-insert)
              ("SPC" . (lambda () (interactive) (progn (corfu-insert) (corfu-insert-separator))))
              ("C-h" . corfu-info-documentation)
              ("M-l" . corfu-info-location)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("TAB" . corfu-next)
              ([tab] . corfu-next)
              ("S-TAB" . corfu-previous)
              ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 0.5)
  (corfu-min-width 5)
  (corfu-max-width 70)
  (corfu-count 14)
  (corfu-scroll-margin 3)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current t)
  (corfu-preselect 'prompt)
  :config
  (require 'corfu)
  ;; Включен глобальный режим автодополнения
  (global-corfu-mode t)
  (corfu-popupinfo-mode)
  (corfu-history-mode)
  
  (defun в-минибуфере-включать-corfu ()
    "Включать Corfu в минибуфере если Vertico/Mct не активны."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1))

;;;; Предпросмотр первого кандидата

;; (use-package corfu-candidate-overlay
;;   :ensure t
;;   :after corfu
;;   :defines (corfu-candidate-overlay-map corfu-mode-map)
;;   :functions (corfu-candidate-overlay-mode)
;;   :bind (:map corfu-mode-map
;;                 ("M-TAB" . corfu-candidate-overlay-complete-at-point))
;;   :hook ((emacs-lisp-mode . corfu-candidate-overlay-mode)
;;        ((typescript-ts-mode . corfu-candidate-overlay-mode)
;;         (js-ts-mode . corfu-candidate-overlay-mode)))
;;   :config

;;   ;;(global-set-key (kbd "M-<tab>") 'completion-at-point)
;;   )

;;;; Расширения для автодополнения

(use-package cape
  :ensure t
  :init)

;;;; Автодополнение для терминала

(use-package corfu-terminal
  :ensure t)

;;;; Предпросмотр дополнения

(use-package completion-preview
  :ensure nil
  :bind (:map completion-preview-active-mode-map
                ("M-f" . #'completion-preview-insert-word)
                ("C-M-f" . #'completion-preview-insert-sexp))
  :custom
  (completion-preview-minimum-symbol-length 2)
  :init
  (global-completion-preview-mode))

(provide 'про-автодополнение)
;;; про-автодополнение.el ends here
