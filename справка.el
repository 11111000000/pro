;;; package --- Summary
;;; Commentary:
;;; Code:
;;; Справка

(use-package help
  :straight (:type built-in)
  )

;;; Дополнительная справка

(use-package helpful 
  :ensure t 
  :defer t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c d" . helpful-at-point)
         ("C-h F" . helpful-function)
         ("C-h C" . helpful-command)))

;;  Изучение API Elisp

(use-package elisp-demos 
  :ensure t 
  :config)

(provide 'справка)
;;; справка.el ends here
