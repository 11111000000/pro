(require 'mule)

(defun toggle-off-input-method ()
  (interactive)
  (if current-input-method (deactivate-input-method))
  )

(use-package god-mode
  :ensure t
  :defer t
  :diminish " ☯"
  :hook (((god-mode-disabled god-mode-enabled) . обновить-курсор)
         ;;(god-mode-enabled . restore-input-method)
         (god-mode-enabled . toggle-off-input-method))

  :bind (("M-i" . god-local-mode)
         ("s-i" . god-local-mode)
         ("<escape>" . god-local-mode)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window)
         ;; ("C-h" . backward-char)
         ("C-j" . next-line)
         ("C-k" . previous-line)
         ;; ("C-l" . forward-char)
         ;; ("C-S-p" . scroll-down-command)
         ;; ("C-S-n" . scroll-up-command)
         :map god-local-mode-map
         ("C-\\" . nil)
         ("i" . god-local-mode)
         ("RET" . (lambda () (interactive)))
         ;; ("h" . backward-char)
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . forward-char)
         ("h" . backward-char)
         ("C-j" . next-line)
         ("C-k" . previous-line)
         ;; ("C-l" . forward-char)
         ;; ("TAB" . forward-word)
         ;; ("H" . (lambda () (interactive) (progn (backward-char 5) (scroll-right 5))))
         ;; ("J" . (lambda () (interactive) (progn (next-line 5) (scroll-up 5))))
         ;; ("K" . (lambda () (interactive) (progn (previous-line 5) (scroll-down 5))))
         ;; ("L" . (lambda () (interactive) (progn (forward-char 5) (scroll-left 5))))
         ;; :map org-mode-map
         ;; ("C-j". next-line)
         ;; ("C-k" . previous-line)
         )

  :custom 
  (god-exempt-major-modes
   '(dired-mode wdired-mode image-mode help-mode grep-mode exwm-mode
     xref--xref-buffer-mode minibuffer-mode help-mode
     vc-annotate-mode eshell-mode shell-mode term-mode
     neotree-mode w3m-mode kite-mini-console-mode mu4e-main-mode
     mu4e-headers-mode mu4e-view-mode browse-kill-ring-mode 
     sx-question-mode Info-mode google-static-mode 
     google-maps-static-mode magit-status-mode magit-popup-mode magit-diff-mode git-commit-mode
     magit-log-mode magit-revision-mode ahg-status-mode
     elfeed-show-mode elfeed-log-mode imenu-mode
     elfeed-search-mode treemacs-mode proced-mode prodigy-mode
     exwm-mode calendar-mode customize-mode diary-mode
     menu-bar-mode docker-mode docker-container-mode 
     docker-image-mode docker-network-mode docker-volume-mode
     package-menu-mode org-agenda-mode calc-mode comint-mode
     racket-repl-mode racket-mode telega-image-mode telega-chat-mode
     lsp-ui-imenu-mode vterm-mode dashboard-mode))

  (god-exempt-predicates (list #'god-exempt-mode-p))
  :init

  (global-set-key (kbd "C-<f1>") help-map)
  (global-set-key (kbd "C-h") help-map)
  
  (defun обновить-курсор ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box 'bar))
    (if (or god-local-mode buffer-read-only) (hl-line-mode 1) (hl-line-mode -1))
    )

  (god-mode-all)
  (обновить-курсор))

(provide 'режим-бога)
