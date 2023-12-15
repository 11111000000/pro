;;; автодополнение.el --- Автодополнение строк
;; Author: Пётр (11111000000@email.com)
;; Version: 1.0
;; Homepage: https://github.com/11111000000/pro
;;; Commentary:
;; Автодополнение текста на базе Corfu
;;; Code:
;;;; Инициализация Corfu

(defun в-минибуфере-включать-corfu ()
    "Включать Corfu в минибуфере если Vertico/Mct не активны."
    (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
        (setq-local corfu-auto nil)
        (corfu-mode 1)))

(use-package corfu
    :bind (:map corfu-map
                  ("<escape>". corfu-quit)
                  ("<return>" . corfu-insert)
                  ("C-h" . corfu-info-documentation)
                  ("M-l" . corfu-info-location)
                  ("C-n" . corfu-quit)
                  ("C-p" . corfu-quit)
                  ("C-n" . corfu-next)
                  ("C-p" . corfu-previous)
                  ("TAB" . corfu-next)
                  ([tab] . corfu-next)
                  ("S-TAB" . corfu-previous)
                  ([backtab] . corfu-previous))
    :custom
    (tab-always-indent 'complete)
    (completion-cycle-threshold nil)
    (corfu-auto nil)
    (corfu-auto-prefix 3)
    (corfu-auto-delay 0.25)
    (corfu-popupinfo-delay 0.7)
    (corfu-min-width 5)
    (corfu-max-width 30)
    (corfu-count 14)
    (corfu-scroll-margin 3)
    (corfu-cycle t)
    (corfu-echo-documentation nil)
    (corfu-separator ?\s)
    (corfu-quit-no-match 'separator)
    (corfu-preview-current 'insert)
    (corfu-preselect 'first)

    :config
    (require 'corfu)
    (global-corfu-mode)
    (corfu-popupinfo-mode)
    (add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1))

;;;; Расширения для автодополнения

(use-package cape
    :ensure t
    :init)

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
    ;; (kind-icon-blend-background nil)
    ;; (kind-icon-blend-frac 0.08)
    :config
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    (add-hook 'kb/themes-hooks #'(lambda ()
                                    (interactive)
                                    (kind-icon-reset-cache))))

;; (defun sorting (list)
;;       (interactive))



(provide 'автодополнение)
;;; автодополнение.el ends here
