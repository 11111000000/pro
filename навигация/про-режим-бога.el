;;; про-режим-бога.el --- Режим Бога (постоянный Ctrl) -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: god-mode, keybinding, ctrl, input-method
;; URL: https://github.com/username/emacs.d/blob/main/навигация/про-режим-бога.el
;;
;;; Commentary:
;;
;; Этот файл настраивает режим Бога в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Режим Бога (god-mode) делает все клавиши эквивалентными
;; нажатым с Ctrl. Это избавляет от необходимости держать палец на Ctrl и
;; значительно ускоряет работу в Emacs, особенно для пользователей QWERTY.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Отключение русской раскладки
;;  2. Настройка god-mode
;;  3. Клавиши и переключение
;;  4. Финал: Provide и ends here
;;
;; Использование: M-x god-mode-all для включения, M-x god-mode для переключения.
;; Рекомендуется подключать после про-редактор.
;;
;;; Code:

(require 'mule)

;;;; Функция для выключения русского

(defun toggle-off-input-method ()
  "Выключение текущего метода ввода."
  (interactive)
  (if current-input-method (deactivate-input-method)))

;;;; Режим Бога (всегда Ctrl)

(defun курсор-бога ()
  "Обновляем курсор."
  (setq cursor-type (if (or god-local-mode buffer-read-only) 'box '(bar . 3)))
  (if (or god-local-mode buffer-read-only) (hl-line-mode 1) (hl-line-mode -1)))

(use-package god-mode
  :ensure t
  :if window-system ;; в консоли отключено, потому что курсоор не меняет цвет
  :defines (god-local-mode-map god-local-mode)
  :functions (god-mode-all)
  :hook (((god-mode-disabled god-mode-enabled) . курсор-бога)
         ;;(god-mode-enabled . restore-input-method)
         (god-mode-enabled . toggle-off-input-method))

  :bind (
         ("M-i" . god-local-mode)
         ("<escape>" . god-local-mode)

         :map god-local-mode-map
         ("C-\\" . nil)
         ("i" . god-local-mode)
         ("`" . self-insert-command)
         ("RET" . (lambda () (interactive)))
         ("j" . next-line)
         ("k" . previous-line)
         ("l" . forward-char)
         ("h" . backward-char)
         ("C-j" . next-line)
         ("C-k" . previous-line)
         ("q" . previous-buffer))

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
                lsp-ui-imenu-mode vterm-mode dashboard-mode helpful-mode eww-mode occur-mode ibuffer-mode
                flymake-diagnostics-buffer-mode profiler-report-mode custom-mode chatgpt-shell-mode
                undo-tree-visualizer-mode yaz-repl messages-buffer-mode context-navigator-view-mode test-flow-panel-mode lore-view-mode
                context-navigator-multifile-mode context-navigator-groups-split-mode atlas-entity-tree-mode))

  (god-exempt-predicates (list #'god-exempt-mode-p (lambda () buffer-read-only)))

  :config

  (global-set-key (kbd "C-<f1>") help-map)
  (global-set-key (kbd "C-h") help-map)

  (god-mode-all)
  (курсор-бога)
  ;; Запретить god-local-mode в read-only буферах и выключать автоматически
  (defun pro/god--deny-in-read-only (orig &optional arg)
    (if buffer-read-only
        (progn
          (when god-local-mode (funcall orig -1))
          (user-error "God-mode запрещён в read-only буферах"))
      (funcall orig arg)))
  (advice-add 'god-local-mode :around #'pro/god--deny-in-read-only)
  (add-hook 'read-only-mode-hook
            (lambda ()
              (when (and buffer-read-only (bound-and-true-p god-local-mode))
                (god-local-mode -1)))))

(provide 'про-режим-бога)
;;; про-режим-бога.el ends here
