;;; про-автодополнение.el --- Автодополнение строк
;;; Commentary:
;; Автодополнение текста на базе Corfu
;;; Code:
;;;; Инициализация Corfu

;; https://github.com/minad/corfu

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
                ("M-S-<iso-lefttab>" . corfu-previous)
                ("M-<tab>" . corfu-next)
                ("TAB" . corfu-next)
                ([tab] . corfu-next)
                ("S-TAB" . corfu-previous)
                ([backtab] . corfu-previous))
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto nil)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0)
  (corfu-popupinfo-delay 0)
  (corfu-min-width 5)
  (corfu-max-width 70)
  (corfu-count 14)
  (corfu-scroll-margin 3)
  (corfu-cycle t)
  (corfu-echo-documentation nil)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)
  (corfu-preview-current t)
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

;;;; Предпросмотр кандидатов

;; https://github.com/minad/corfu-candidate-overlay
(use-package corfu-candidate-overlay
  :ensure t
  :after corfu
  :functions (corfu-candidate-overlay-mode)
  :config
  (corfu-candidate-overlay-mode +1)
  ;; bind Ctrl + TAB to trigger the completion popup of corfu
  (global-set-key (kbd "M-<tab>") 'completion-at-point)
  ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
  ;; (keybing <iso-lefttab> may not work for your keyboard model)
  ;(global-unset-key (kbd "<tab>") 'corfu-candidate-overlay-complete-at-point)
  )

;;;; Расширения для автодополнения

(use-package cape
  :ensure t
  :init)

;;;; Автодополнение для терминала

(use-package corfu-terminal
  :ensure t)

;;;; Сниппеты

;; Быстрые шаблоны - сниппеты можно создавать на лету со шпаргалкой

(defvar шаблон-для-сниппета (concat
                             (file-name-directory
                              (locate-library "про-код"))
                             "etc/шаблон-сниппета.txt"))

(defun создать-новый-сниппет-со-шпаргалкой ()
  "Создать новый сниппет со шпаргалкой."
  (interactive)
  (funcall-interactively 'yas-new-snippet)
  (erase-buffer)
  (insert-file-contents шаблон-для-сниппета))

(use-package yasnippet
  :ensure t
  :functions (yas-reload-all)
  :defines (yas-snippet-dirs)
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all)
  (setq yas-snippet-dirs
       '("~/.emacs.d/snippets")))

;; Функция автодополнения для сниппетов

(use-package yasnippet-capf
  :ensure t
  :defines (completion-at-point-functions)
  :after cape
  :init
  (defun про/yasnippet-capf-h ()
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  :hook
  ((emacs-lisp-mode . про/yasnippet-capf-h)
   (js-ts-mode . про/yasnippet-capf-h))
  )

(use-package yasnippet-snippets
  :ensure t
  :init)


(provide 'про-автодополнение)
;;; про-автодополнение.el ends here
