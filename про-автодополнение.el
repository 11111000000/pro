;;; про-автодополнение.el --- Автодополнение строк
;; Author: Пётр (11111000000@email.com)
;; Version: 1.0
;; Homepage: https://github.com/11111000000/pro
;;; Commentary:
;; Автодополнение текста на базе Corfu
;;; Code:
;;;; Инициализация Corfu

(use-package corfu
  :ensure t
  :defines (corfu-map)
  :functions (corfu-mode global-corfu-mode corfu-popupinfo-mode corfu-history-mode)
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
  (corfu-auto-prefix 2)
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
  (corfu-preselect 'promt)

  :config
  (require 'corfu)
  (global-corfu-mode)
  (corfu-popupinfo-mode)
  (corfu-history-mode)

  (defun в-минибуфере-включать-corfu ()
    "Включать Corfu в минибуфере если Vertico/Mct не активны."
    (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
      (setq-local corfu-auto nil)
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'в-минибуфере-включать-corfu 1))

;;;; Расширения для автодополнения

(use-package cape
  :ensure t
  :init)

;;;; Автодополнение для терминала

(use-package corfu-terminal
  :ensure t)



(provide 'про-автодополнение)
;;; автодополнение.el ends here
