;;; package --- Summary
;;; Commentary:
;;; Code:

(modify-all-frames-parameters '((inhibit-double-buffering . t)))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(global-set-key (kbd "s-w") 'delete-window)
(global-set-key (kbd "C-w") 'delete-window)

(global-set-key (kbd "s-d") 'nil)
(global-set-key (kbd "s-t") 'make-frame)

(global-set-key (kbd "s-q") 'delete-frame)

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode -1)
  (setq smooth-scroll/vscroll-step-size 10))

(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-+") 'text-scale-increase)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s-_") 'text-scale-set)

;; (add-hook 'window-setup-hook (lambda()
;;                                  (setq ns-auto-hide-menu-bar t)
;;                                  (set-frame-position nil 0 -24)
;;                                  (set-frame-size nil (display-pixel-width) (display-pixel-height) t)))


(provide 'огрызок)
