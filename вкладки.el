;;; вкладки.el --- Вкладки
;; Вкладки
;;; Commentary:
;;; Code:
;;;;  Верхний уровень вкладок

(use-package powerline
  :ensure t)


(use-package tab-bar
  :ensure t
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-separator " ")
  (tab-bar-auto-width-min '(1 2))
  (tab-bar-auto-width-min '(30 10))
  (tab-bar-auto-width nil(tab-bar-auto-width-min '(1 2))
  (tab-bar-auto-width-min '(30 10))
  (tab-bar-auto-width t))
  :config

  (defvar высота-вкладки 22)

  (dotimes (i 10)
    (global-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i))))
  
  (require 'powerline)
  
  (defvar левая-часть-вкладки (powerline-wave-right 'tab-bar nil высота-вкладки))
  (defvar правая-часть-вкладки (powerline-wave-left nil 'tab-bar высота-вкладки))
  
  (defun формат-вкладки-tab-bar (tab i)
    (let* ((вкладка-текущая? (eq (car tab) 'current-tab))
          (буфер-вкладки (alist-get 'name tab))
          (иконка-режима (all-the-icons-icon-for-mode (with-current-buffer буфер-вкладки major-mode) :height 0.8))
          (фейс-текущей-вкладки (if вкладка-текущая? 'tab-bar-tab 'tab-bar-tab-inactive ))
          (имя-буфера (substring-no-properties (alist-get 'name tab)))
          (имя-вкладки (format "%s" (if (> (length имя-буфера) 21)
                                        (concat
                                         (substring имя-буфера 0 21) "…")
                                      имя-буфера)))
          (текст-вкладки (concat
                          " "
                          (if (symbolp иконка-режима) "?" иконка-режима)
                          " "
                          имя-вкладки
                          " ")))
      (add-face-text-property 0 (length текст-вкладки) фейс-текущей-вкладки t текст-вкладки)
      текст-вкладки))
  
  (setq tab-bar-tab-name-format-function #'формат-вкладки-tab-bar)
  (setq tab-bar-tab-name-function #'tab-bar-tab-name-current); #'имя-вкладки-tab-bar)

  (tab-bar-mode t))

(defun открыть-новую-вкладку ()
  "Открыть новую вкладку с дашбордом."
  (interactive)
  (tab-bar-new-tab-to)
  (dashboard-open))

;;;;;  Вкладки уровня окна

(use-package tab-line
  :custom
  (tab-line-new-button-show nil)
  (tab-line-close-button-show nil)
  (tab-line-separator "")
  (tab-line-switch-cycling t)
  (tab-line-tabs-function 'tab-line-tabs-mode-buffers)
  :hook ((vterm-mode . tab-line-mode)
         (telega-mode . tab-line-mode))
  :bind (("M-s-n" . #'tab-line-switch-to-next-tab)
         ("M-s-p" . #'tab-line-switch-to-prev-tab)
         ("M-S-n" . #'tab-line-switch-to-next-tab)
         ("M-S-p" . #'tab-line-switch-to-prev-tab))
  :config

  (defvar высота-tab-line 22)
;;;;; 
  (require 'powerline)

  (defun формат-имени-вкладки-tab-line (buffer &optional _buffers)
    (powerline-render (list (powerline-wave-right 'tab-line nil высота-tab-line)
                            (format "%s" (buffer-name buffer))
                            (powerline-wave-left nil 'tab-line высота-tab-line))))
  
  (setq tab-line-tab-name-function #'формат-имени-вкладки-tab-line)

  (global-tab-line-mode -1))

;; (use-package project-tab-groups
;;   :ensure
;;   :config
;;   (project-tab-groups-mode 1))

(defun закрыть-вкладку-и-буфер ()
  "Закрывает вкладку и буфер в ней."
  (interactive)
  (kill-this-buffer)
  (tab-close))

(provide 'вкладки)
;;; вкладки.el ends here
