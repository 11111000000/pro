;;; Редактор
;;; Кодировка

;;; Code:
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq locale-coding-system 'utf-8
      default-file-name-coding-system 'utf-8
      x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;;;  Клавиатура

;; Язык клавиатуры

(setq default-input-method "russian-computer")

;; Трансляция сочетаний клавиш при включеном русском

(use-package reverse-im
  :ensure t
  :config
  (add-to-list 'reverse-im-modifiers 'super)
  (add-to-list 'reverse-im-input-methods "russian-computer")
  (reverse-im-mode t))

;;;  Курсор

;; Курсор представляет из себя мигающий прямоугольник, ширина которого зависит от размера символа под ним

(setq cursor-type 'box)
(blink-cursor-mode t)
(setq x-stretch-cursor t)

;; В зависимости от включенного режима ввода, курсор меняет свой вид

(use-package cursor-chg
  :ensure t
  :straight '(cursor-chg :host github :repo "emacsmirror/cursor-chg"))

(setq curchg-input-method-cursor-color "red"
      curchg-default-cursor-type 'bar
      curchg-default-cursor-color "#333"
      curchg-change-cursor-on-input-method-flag t)

(change-cursor-mode t)

;; Можно прыгнуть сразу на любой символ, нажав C-z и этот символ

(use-package avy
  :ensure t
  :defer t
  :bind   (("C-z" . avy-goto-char)
           ("s-z" . avy-goto-char)
           ("M-z" . avy-goto-char))
  :custom ((avy-background nil)))

;; Мульти-курсор

(use-package multiple-cursors
  :ensure t
  :bind (("C-c SPC" . mc/mark-all-like-this)
         ("C-c C-SPC" . mc/unmark-all-like-this)
         ("C-c <mouse-1>" . mc/add-cursor-on-click)))

;; При перемещении в начало строки *<C-a>*, сперва прыгать к  идентации, затем - к началу строки

(defun back-to-indentation-or-beginning ()
  "вернуться к идентации или началу строки."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Подсвечивать курсор при мгновенном перемещении, чтобы он не потерялся

;; (use-package beacon
;;   :custom
;;   (beacon-color "#ffaa00")
;;   :hook (after-init . beacon-mode))

;;;  Выделение

;; http://www.cs.man.ac.uk/~chl/secondary-selection.html
;; https://emacs.stackexchange.com/questions/17056/what-is-the-origin-of-the-term-yank

;; Выделять "изнутри"

(use-package expand-region
  :ensure t
  :bind
  ("M-SPC" . er/expand-region)
  ("M-S-SPC" . er/contract-region)
  ("S-SPC" . mark-current-line)
  :config
  (setq expand-region-contract-fast-key "M-S-SPC"
        expand-region-reset-fast-key    "<ESC><ESC>"))

;; Выключены *C-d* и *C-w*, весьма деструктивные.

(global-unset-key (kbd "C-d"))
(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "s-x"))
(global-unset-key (kbd "M-c"))

;; Операции удаления (Vim-стиль)

(bind-keys*
 ("C-d C-f" . delete-forward-char)
 ("C-d C-b" . backward-dnelete-char-untabify)
 ("C-d C-e" . kill-line)
 ("C-d C-w" . delete-trailing-whitespace)
 ("C-d C-a" . delete-to-begin))

;; Добавление линии над и под

(bind-keys*
 ("M-o" . vi-open-line)
 ("C-M-o" . vi-open-line) 
 ("M-O" . vi-open-line-above)
 ("C-M-O" . vi-open-line-above))

;; Операция над выделением или текущей линией

(use-package whole-line-or-region
  :ensure t
  :bind (("M-y" . whole-line-or-region-kill-ring-save) ;; TODO Пофиксить в org
         ("s-y" . whole-line-or-region-kill-ring-save) ;; TODO Пофиксить в org
         ;;("C-y" . whole-line-or-region-yank)
         ("C-d C-d" . whole-line-or-region-kill-region)
         ("C-M-d" . whole-line-or-region-kill-region)))

;; Функция delete-to-begin, для вырезания от курсора до начала строки:

(defun delete-to-begin ()
  "Удалить до начала строки."
  (interactive) (kill-line 0))

;;;  Поиск и замена

(global-set-key (kbd "M-r") 'replace-string)
(global-set-key (kbd "M-R") 'replace-regexp)
(global-set-key (kbd "C-c C-r") 'replace-string)
(global-set-key (kbd "C-c r") 'replace-regexp)
(global-set-key (kbd "C-c M-r") 'replace-regexp)

;;;  Перемещение блоков

(use-package shift-text
  :ensure t
  :defer t
  :bind
  ("C-S-M-p" . shift-text-up)
  ("C-S-M-n" . shift-text-down)
  ("C-S-M-f" . shift-text-right)
  ("C-S-M-b" . shift-text-left))

;;;  Отображения текста по центру ("режим чтения")

(use-package olivetti
  :ensure t
  :hook ((text-mode Man-mode Info-mode) . olivetti-mode)
  :custom ((olivetti-minimum-body-width 80)))

;;;  Закладки

(use-package bookmark
  :config
  (setq bookmark-save-flag t))

;;;  Режим чтения

;; (use-package view
;;   :bind (
;;          ("M-i" . read-only-mode)
;;          ("<escape>" . read-only-mode)
;;          ("C-h" . backward-char)
;;          ("C-j" . next-line)
;;          ("C-k" . previous-line)
;;          ("C-l" . forward-char)
;;          :map view-mode-map
;;          ("i" . read-only-mode)
;;          ("<backspace>" . nil)
;;          ("DEL" . nil)
;;          ("SPC" . nil)
;;          ("j" . next-line)
;;          ("k" . previous-line)
;;          ("n" . next-line)
;;          ("p" . previous-line)
;;          ("h" . backward-char)
;;          ("l" . forward-char)
;;          )
;;   :custom ((view-read-only -1))
;;   :hook (((text-mode prog-mode emacs-lisp-mode) . read-only-mode))
;;   :init
;;   )

;;;  Красивые типографские символы

;; Последовательности символов можно заменить на один глиф. Но при наведении курсора, мы хотим видеть оригинал:

(use-package fira-code-mode :ensure t
  :ensure t
  :hook ((prog-mode . fira-code-mode))
  :config
  (fira-code-mode-set-font))

(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point t)

;;;  Переносы

(setq-default truncate-lines t
              truncate-partial-width-windows 50
              longlines-show-hard-newlines t
              line-move-visual t)

(toggle-truncate-lines t)
(visual-line-mode t)
(global-set-key (kbd "C-$") 'toggle-truncate-lines)

;;;  Конфигурация отступов

;; По умолчанию отступы в 2 пробела

(setq-default  indent-tabs-mode nil
               tab-width 2
               indent-line-function 'indent-relative
               default-tabs-width 2)

;; Автоматически выравнивать при переводе строки

(electric-indent-mode t)

;; Автоматически определять отступы

(use-package dtrt-indent
  :ensure t
  :init
  (dtrt-indent-global-mode t)
  )

;; Настройка отступов берётся из файла .editorconfig

(use-package editorconfig
  :ensure t
  :init
  (editorconfig-mode 1))

;;;  Сравнение

;; Плоское, горизонтальное расположение при сравнении буферов

(setq-default ediff-window-setup-function 'ediff-setup-windows-plain
              ediff-split-window-function 'split-window-horizontally)

;;;  Редактировать как Root

;; Функция edit-current-file-as-root позволяет легко открыть
;; текущий файл с правами root

(defun edit-current-file-as-root () "Edit as root the file associated with the current buffer"
       (interactive)
       (if (buffer-file-name)
           (progn
             (setq file (concat "/sudo:root@localhost:" (buffer-file-name)))
             (find-file file))
         (message "Buffer is not associated to a file.")))

(use-package string-inflection
  :ensure t
  ;;:load-path "emacs-lisp/string-inflection"
  )

;; Подсветка идентации

(use-package highlight-indent-guides
   :ensure t
   :hook ((prog-mode . highlight-indent-guides-mode)
          (yaml-mode . highlight-indent-guides-mode))
   :custom
   (highlight-indent-guides-method 'character)
   (highlight-indent-guides-responsive 'top)
   (highlight-indent-guides-auto-character-face-perc 5)
   )

;; Поддержка очень длинных файлов

(global-so-long-mode t)

;; Проверка орфографии

(use-package flymake-aspell
  :ensure t
  :hook ((text-mode . flymake-aspell-setup)
         (org-mode . flymake-aspell-setup))
  :init
  (setq ispell-dictionary "ru")
  (setq ispell-program-name "aspell")
  (setq ispell-silently-savep t))

;;; Outshine

(use-package outshine  
  :ensure t
  :hook (((prog-mode emacs-lisp-mode js-mode) . outline-minor-mode)
         (outline-minor-mode . outshine-mode)
         (outline-minor-mode . iimage-mode))
  :bind (:map outshine-mode-map
              ("C-<return>" . outshine-insert-heading)
              ("C-<tab>" . outshine-cycle)))

(use-package outshine-bullets
  :disabled t 
  :straight '(outshine-bullets :host github :repo "alphapapa/outshine-bullets")
  :ensure
  ;; :load-path "emacs-lisp/outshine-bullets"
  :hook ((outshine-mode . outshine-bullets-mode))
  :custom (
	   (outshine-bullets-bullet-list '("‣" "‣" "‣" "‣" "‣"))
	   ))

(use-package markdown-mode :ensure t)

(provide 'редактор)
;;; dobro-editor.el ends here
