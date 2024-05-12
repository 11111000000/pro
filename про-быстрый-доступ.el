;;; про-быстрый-доступ.el --- Быстрый поиск имён в вертикальных списках
;;; Commentary:
;;; Code:

;;;; Поиск по файлу

;; TODO: Слишком агрессивно мапит клавиши, заменить отдельные?
;; (use-package ctrlf
;;   :ensure t
;;   :defines (ctrlf-default-search-style ctrlf-alternate-search-style)
;;   :functions (ctrlf-mode)
;;   :config
;;   (setq ctrlf-default-search-style 'fuzzy-regexp)
;;   (setq ctrlf-alternate-search-style 'literal)
;;   ;;(setq ctrlf-default-search-style 'literal)
;;   (ctrlf-mode -1))


;;;; Вертикальные списки

(use-package vertico
  :ensure t
  :defines (vertico-map)
  :functions (vertico-mode)
  :custom
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (vertico-cycle t)
  (completion-ignore-case t)
  :bind (:map vertico-map
                ("s-<tab>" . vertico-next)
                ("s-<iso-lefttab>" . vertico-previous))
  :init
  (require 'vertico)
  :config
  (vertico-mode t))

;;;; Сортировка

(use-package orderless
  :ensure t
  :config (setq completion-styles '(orderless basic) completion-category-defaults nil completion-category-overrides
               '((file
                  (styles partial-completion)))))

;;;; Подписи и дополнительная информация

(use-package marginalia
  :ensure t
  :functions (marginalia-mode)
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config (marginalia-mode))

;;;; Функции на базе автодополения

(use-package consult
  :ensure t
  :defines (consult-project-root-function)
  :functions (consult-customize consult-xref)
  :bind
  (([remap bookmark-jump] . consult-bookmark)
   ([remap goto-line] . consult-goto-line)
   ([remap imenu] . consult-imenu)
                                        ;([remap isearch-forward] . consult-line)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ([remap repeat-complex-command] . consult-complex-command)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap yank-pop] . consult-yank-pop)
                                        ;("C-S-r" . isearch-backward-regexp)
                                        ;("C-S-s" . isearch-forward-regexp)
   :map consult-narrow-map
   ("C-h" . consult-narrow-help)
   :map goto-map
   ("m" . consult-mark)
   ;; TODO Use `consult-org-heading' in `org-mode'
   ("o" . consult-outline)
   :map help-map
   ("M" . consult-minor-mode-menu))
  :custom ((consult-preview-key "M-.")
          (consult-line-start-from-top t)
          (xref-show-definitions-function #'consult-xref)
          (xref-show-xrefs-function #'consult-xref))
  :config
  (autoload 'projectile-project-root "projectile")
  (require 'consult-xref)
  (setq consult-project-root-function #'projectile-project-root))

;;;; Дополнение сниппетов

(use-package consult-yasnippet
  :ensure t)

;;;; Поиск по документации (dash)

(use-package consult-dash
  :ensure t
  :defines (consult-dash)
  :config (consult-customize consult-dash
                             :initial (thing-at-point 'symbol)))

;;;; Поиск файлов

(require 'dired)

(use-package consult-ag
  :ensure t
  :after dired
  :functions (consult-ag)
  :config
  (defun искать-по-файлам-отсюда ()
    "Поиск по файлам от текущего пути."
    (interactive)
    (consult-ag default-directory))
                                        ; Эту функцию нужно добавить, т.к. она пропала в новых версиях consult

  
  (defun consult--position-marker (buffer line column)
    "Get marker in BUFFER from LINE and COLUMN."
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-restriction
          (save-excursion
            (widen)
            (goto-char (point-min))
            ;; Location data might be invalid by now!
            (ignore-errors
              (forward-line (- line 1))
              (forward-char column))
            (point-marker))))))

  :bind (:map dired-mode-map
                ("s" . искать-по-файлам-отсюда)))

;;;; Поиск по языковым серверам

(use-package consult-lsp
  :after (lsp consult)
  :ensure t
  :disabled t)

(use-package consult-eglot
  :after (eglot consult)
  :defines (eglot-mode-map)
  :functions (consult-eglot-symbols)
  :ensure t
  :bind (:map eglot-mode-map ("C-c C-." . #'consult-eglot-symbols)))

(provide 'про-быстрый-доступ)
;;; быстрый-доступ.el ends here
