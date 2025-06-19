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
  (corfu-auto-prefix 3)
  (corfu-auto-delay 0.4)
  (corfu-popupinfo-delay 0.5)
  (corfu-min-width 5)
  (corfu-max-width 70)
  (corfu-count 14)
  (corfu-scroll-margin 3)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current nil)
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

(defun pro/disable-ispell-capf ()
  "Убрать `ispell-completion-at-point' из списка CAPF."
  (setq-local completion-at-point-functions
              (remq #'ispell-completion-at-point
                    completion-at-point-functions)))

;(pro/disable-ispell-capf) ;; выключаем capf+ispell (тормозит)
(add-hook 'text-mode-hook #'pro/disable-ispell-capf)
(add-hook 'prog-mode-hook #'pro/disable-ispell-capf) ; если тормозит и в коде

;; Альтернатива:
;; (with-eval-after-load 'cape
;;   (add-to-list 'completion-at-point-functions #'cape-dict))




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

;; (use-package completion-preview
;;   :ensure nil
;;   :functions (completion-preview-insert-word completion-preview-insert-sexp)
;;   :bind (:map completion-preview-active-mode-map
;;                 ("M-f" . #'completion-preview-insert-word)
;;                 ("C-M-f" . #'completion-preview-insert-sexp))
;;   :custom
;;   (completion-preview-minimum-symbol-length 3)
;;   :init
;;   (add-hook 'text-mode-hook (lambda () (completion-preview-mode -1)))
;;   (global-completion-preview-mode))


(provide 'про-автодополнение)
;;; про-автодополнение.el ends here
