;;; вкладки.el --- Вкладки
;; Вкладки
;;; Commentary:
;;; Code:
;;;;  Верхний уровень вкладок

(use-package powerline
  :ensure t)

(require 'powerline)
(defvar добрая-высота-вкладки 22)
(defvar добрая-вкладка-слева (powerline-wave-right 'tab-bar nil добрая-высота-вкладки))
(defvar добрая-вкладка-справа (powerline-wave-left nil 'tab-bar добрая-высота-вкладки))

(defvar список-кружков-с-цифрами
    '((0 . "⓪")
      (1 . "①")
      (2 . "②")
      (3 . "③")
      (4 . "④")
      (5 . "⑤")
      (6 . "⑥")
      (7 . "⑦")
      (8 . "⑧")
      (9 . "⑨"))
    "Alist of integers to strings of circled unicode numbers.")

(defun добрый-формат-имени-вкладки (tab i)
  (let ((current-p (eq (car tab) 'current-tab))
       (tab-num (if (and tab-bar-tab-hints (< i 10))
                    (alist-get i список-кружков-с-цифрами) "")))
    ;; (propertize
    ;;  (powerline-render (list добрая-вкладка-слева
    ;;                          (format "%s" (alist-get 'name tab))
    ;;                          добрая-вкладка-справа
    ;;                          ))
    ;;  'face (funcall tab-bar-tab-face-function tab))
    (propertize
     (concat
      " "
      tab-num
      " "
      (alist-get 'name tab)
      " ")
     'face (funcall tab-bar-tab-face-function tab))
    ))

(use-package tab-bar
  :ensure t
  :custom
  (tab-bar-new-button-show nil)
  (tab-bar-close-button-show nil)
  (tab-bar-separator " ")
  :config
  (tab-bar-mode t)
  (setq-default tab-bar-close-button nil)

  (setq tab-bar-tab-name-function #'tab-bar-tab-name-current-with-count)
  (setq tab-bar-tab-name-format-function #'добрый-формат-имени-вкладки)
  (setq tab-bar-tab-hints t)

  (dotimes (i 10)
    (global-set-key (kbd (format "s-%d" i)) `(lambda () (interactive) (tab-bar-select-tab ,i)))))

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
  :hook ((vterm-mode . tab-line-mode))
  :bind (("M-s-n" . #'tab-line-switch-to-next-tab)
            ("M-s-p" . #'tab-line-switch-to-prev-tab))
  :config
  
  (global-tab-line-mode -1)
  
  (require 'powerline)
  
  (defvar az/tab-height 22)
  (defvar az/tab-left (powerline-wave-right 'tab-line nil az/tab-height))
  (defvar az/tab-right (powerline-wave-left nil 'tab-line az/tab-height))

  (defun az/tab-line-tab-name-buffer (buffer &optional _buffers)
    (powerline-render (list az/tab-left
                            (format "%s" (buffer-name buffer))
                            az/tab-right)))
  (setq tab-line-tab-name-function #'az/tab-line-tab-name-buffer))

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
