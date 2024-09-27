;;; про-режим-бога.el --- Режим бога, постоянный Контрол -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'mule)

;;;; Функция для выключения русского

(defun toggle-off-input-method ()
  "Выключение текущего метода ввода."
  (interactive)
  (if current-input-method (deactivate-input-method)))

;;;; Режим Бога (всегда Ctrl)

(use-package god-mode
  :if window-system ;; в консоли отключено, потому что курсоор не меняет цвет
  :defines (god-local-mode-map god-local-mode)
  :functions (god-mode-all)
  :ensure t
  :hook (((god-mode-disabled god-mode-enabled) . обновить-курсор)
       ;;(god-mode-enabled . restore-input-method)
       (god-mode-enabled . toggle-off-input-method))

  :bind (
         ("M-i" . god-local-mode)
         ("<escape>" . god-local-mode)

         :map god-local-mode-map
         ("C-\\" . nil)
         ("i" . god-local-mode)
         ("RET" . (lambda () (interactive)))
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . forward-char)
         ("h" . backward-char)
         ("C-j" . next-line)
         ("C-k" . previous-line)
         ("q" . kill-current-buffer)
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
                racket-repl-mode racket-mode telega-image-mode telega-chat-mode telega-root-mode
                lsp-ui-imenu-mode vterm-mode dashboard-mode helpful-mode eww-mode occur-mode ibuffer-mode))

  (god-exempt-predicates (list #'god-exempt-mode-p))

  :config

  (global-set-key (kbd "C-<f1>") help-map)
  (global-set-key (kbd "C-h") help-map)

  (defun обновить-курсор ()
    (setq cursor-type (if (or god-local-mode buffer-read-only) 'box '(bar . 3)))
    (if (or god-local-mode buffer-read-only) (hl-line-mode 1) (hl-line-mode -1)))

  (god-mode-all)
  (обновить-курсор))

(provide 'про-режим-бога)
;;; про-режим-бога.el ends here
