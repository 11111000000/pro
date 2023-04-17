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
  (tab-bar-auto-width nil)
  :config
  (setq иконка-по-умолчанию (all-the-icons-octicon "browser" :height 1  :v-adjust 0.1))
  (setq иконка-firefox (all-the-icons-faicon "firefox" :height 1  :v-adjust 0))
  (setq иконка-chrome (all-the-icons-faicon "chrome" :height 1  :v-adjust 0))
  (setq иконка-telegram (all-the-icons-faicon "comment" :height 1  :v-adjust 0))
  (setq высота-вкладки 18)
  (setq длинна-имени-вкладки 25)
   (setq замены-имён-вкладки `(("Firefox-esr" . ,иконка-firefox)
                                   ("Google-chrome" . ,иконка-chrome)))

  (dotimes (i 10)
    (global-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i))))

  ;(defvar левая-часть-вкладки (all-the-icons-alltheicon "wave-right"))
                                        ;(defvar правая-часть-вкладки (powerline-wave-left nil 'tab-bar высота-вкладки))


  (defun заменить-строки (пары-для-замены строка)
    (seq-reduce
     (lambda (строка пара)
       (string-replace
        (car пара)
        (cdr пара)
        строка))
     пары-для-замены
     строка))

   (defun find-buffer-element (list)
     (loop for element in list
           (pp element)
           ))


  (defun формат-вкладки-tab-bar (tab i)
    (let* ((вкладка-текущая? (eq (car tab) 'current-tab))
          (имя-буфера (substring-no-properties (alist-get 'name tab)))
          (режим-вкладки (if (bufferp (get-buffer имя-буфера)) (with-current-buffer имя-буфера major-mode) nil))
          (иконка-режима (all-the-icons-icon-for-mode режим-вкладки :height 0.8))
          (иконка-вкладки (if (symbolp иконка-режима) иконка-по-умолчанию иконка-режима))
          (фейс-текущей-вкладки (if вкладка-текущая? 'tab-bar-tab 'tab-bar-tab-inactive))
          (укороченое-имя  (заменить-строки замены-имён-вкладки
                                            имя-буфера))
          (имя-вкладки (format "%s" (if (> (length укороченое-имя) длинна-имени-вкладки)
                                      (concat
                                       (substring укороченое-имя 0 длинна-имени-вкладки) "…")
                                    укороченое-имя)))
          (текст-вкладки (concat
                          ;(all-the-icons-alltheicon "wave-right" :height 1.3 :v-adjust 0 :face `(face :foreground ,(face-attribute 'tab-bar :background)))
                          " "
                          иконка-вкладки
                          " "
                          имя-вкладки
                          " "
                          ;(all-the-icons-alltheicon "wave-left" :height 1.3 :v-adjust -0.1 :face `(face :foreground ,(face-attribute 'tab-bar :background)))
                          )))
      ;(pp буфер-вкладки)
      ;(pp иконка-режима)
      (add-face-text-property 0 (length текст-вкладки) фейс-текущей-вкладки t текст-вкладки)
      текст-вкладки))

  (setq tab-bar-tab-name-format-function  #'формат-вкладки-tab-bar)
  (setq tab-bar-tab-name-function #'tab-bar-tab-name-current)

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
