;;; справка.el --- Справка и подсказки
;;; Commentary:
;;; Code:
;;;; Справка

(leaf help  
  )

;;;; Info

(leaf info   
  :bind ((:Info-mode-map
         ("DEL" . Info-history-back)
         ("B" . Info-history-back)
         ("F" . Info-history-forward)
         ("h" . Info-up)
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . Info-follow-nearest-node)
         ("<XF86Back>" . nil)
         ("<XF86Forward>" . nil))))

;;;; Дополнительная справка

(leaf helpful 
  :ensure t 
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h ." . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;;;; Изучение API Elisp

(leaf elisp-demos 
  :ensure t 
  :config)

;;;; Подсказка комбинаций кавиш

(leaf guide-key
  :ensure t  
  :diminish " C-?"
  :custom
  ((guide-key/guide-key-sequence . '("C-x" "C-c" "ESC" "C-," "C-z" "C-t" "C-." "s-p"
                                   "M-t" "M-g" "SPC" "C-d" "F1" "M-s"))
   (guide-key/popup-window-position . 'top)
   (guide-key/recursive-key-sequence-flag . t)
   (guide-key/idle-delay . 1.7)
   (guide-key/text-scale-amount . -1)
   (guide-key/highlight-prefix-regexp . "Prefix")
   (guide-key/highlight-command-regexp .
    '("rectangle"
      ("buffer" . "sky blue")
      ("org" . "cornflower blue")
      ("outshine" . "Dark Violet")
      ("helm" . "lime green")
      ("consult" . "lime green")
      ("popwin" . "hot pink")
      ("macro" . "medium orchid")
      ("region" . "cadet blue")
      ("mark" . "moccasin"))))
  :init
  (guide-key-mode t))

;;;; Документация по языку во всплывающем окне

(leaf eldoc-box
  :ensure t
  :custom
  (eldoc-idle-delay . 1)
  
  :hook ((emacs-lisp-mode . eldoc-box-hover-mode)
         (prog-mode . eldoc-box-hover-mode)
         (eglot-managed-mode-hook . eldoc-box-hover-mode)
         (eldoc-box-frame-hook . (lambda ()
                                   (setq cursor-in-non-selected-windows nil))))
  :config
  (require 'eldoc)
  (setq-default cursor-in-non-selected-windows nil)
  (setq-default eldoc-documentation-strategy #'eldoc-documentation-enthusiast)
  (setq-default eldoc-documentation-function #'eldoc-documentation-enthusiast)
  (set-face-attribute 'eldoc-box-border nil :background (face-foreground 'font-lock-comment-face))
  (set-face-attribute 'eldoc-box-body nil :family "Fira Code" :weight 'normal :italic nil :height 0.8))

;;;; Статистика нажатий

(leaf keyfreq
  :ensure t
  :config
  (keyfreq-mode)
  (keyfreq-autosave-mode))

(provide 'справка)
;;; справка.el ends here
