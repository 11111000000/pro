;;; про-справку.el --- Справка и подсказки -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;;; Info

(use-package info
  :defer t
  :bind (:map Info-mode-map
                ("DEL" . Info-history-back)
                ("B" . Info-history-back)
                ("F" . Info-history-forward)
                ("h" . Info-up)
                ("j" . next-line)
                ("k" . previous-line)
                ("l" . Info-follow-nearest-node)
                ("<XF86Back>" . nil)
                ("<XF86Forward>" . nil)))

;;;; Дополнительная справка

(use-package helpful
  :ensure t
  :defer t
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ("C-h ." . helpful-at-point))
  

;;;; Изучение API Elisp

(use-package elisp-demos
  :defer t
  :ensure t
  :functions (elisp-demos-advice-helpful-update)
  :config
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;; Подсказка комбинаций кавиш

(use-package guide-key
  :defer t
  :ensure t
  :diminish " C-?"
  :custom
  ((guide-key/guide-key-sequence '("C-x" "C-c" "ESC" "C-," "C-z" "C-t" "C-." "M-t" "M-g" "SPC" "C-d" "F1" "M-s" "C-h"))
   (guide-key/popup-window-position 'top)
   (guide-key/recursive-key-sequence-flag t)
   (guide-key/idle-delay 3)
   (guide-key/text-scale-amount -1)
   (guide-key/highlight-prefix-regexp "Prefix")
   (guide-key/highlight-command-regexp
    '("rectangle"
      ("buffer" . "sky blue")
      ("org" . "cornflower blue")
      ("outshine" . "dark violet")
      ("helm" . "lime green")
      ("consult" . "lime green")
      ("popwin" . "hot pink")
      ("macro" . "medium orchid")
      ("region" . "cadet blue")
      ("mark" . "moccasin"))))
  :init
  (guide-key-mode t))

;;;; Документация по языку во всплывающем окне

(use-package eldoc-box
  :ensure t
  :functions (eldoc-box-hover-mode)
  :bind (("M-/" . eldoc-box-help-at-point)) ;; TODO: Перенести в org
  :custom ((eldoc-idle-delay 0.2)
          (eldoc-box-offset '(-40 50 -70))
          (eldoc-idle-delay 1.0))
  :config
  (require 'eldoc)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (eldoc-box-hover-mode t))

;;;; Статистика нажатий

(use-package keyfreq
  :defer t
  :ensure t
  :functions (keyfreq-mode keyfreq-autosave-mode)
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode))


(provide 'про-справку)
;;; про-справку.el ends here
