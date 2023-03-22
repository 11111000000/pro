;;; справка.el --- Справка и подсказки
;;; Commentary:
;;; Code:

;;;; Info

(use-package info
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
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h ." . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;;;; Изучение API Elisp

(use-package elisp-demos
  :ensure t
  :config)

;;;; Подсказка комбинаций кавиш

(use-package guide-key
  :ensure t
  :defer t
  :diminish " C-?"
  :custom
  ((guide-key/guide-key-sequence '("C-x" "C-c" "ESC" "C-," "C-z" "C-t" "C-." "M-t" "M-g" "SPC" "C-d" "F1" "M-s"))
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
  :bind ("C-c i" . eldoc-doc-buffer)
  :custom ((eldoc-idle-delay 1) (eldoc-box-offset '(-50 50 -50)))
  :hook ((emacs-lisp-mode . eldoc-box-hover-mode)
         (prog-mode . eldoc-box-hover-mode)
         (eglot-managed-mode-hook . eldoc-box-hover-mode)
         (eldoc-box-frame-hook . (lambda ()
                                   (toggle-truncate-lines t)
                                   (tab-bar-mode -1)
                                   (setq cursor-in-non-selected-windows nil))))
  :config
  (require 'eldoc)
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default eldoc-documentation-strategy #'eldoc-documentation-default)
  (setq-default eldoc-documentation-function #'eldoc-documentation-default)
  (set-face-attribute 'eldoc-box-border nil :background (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'eldoc-box-body nil :family "Fira Code" :weight 'normal :italic nil :height 1.0))

;;;; Статистика нажатий

(use-package keyfreq
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))


(provide 'справка)
;;; справка.el ends here
