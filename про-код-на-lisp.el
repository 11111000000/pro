;;; про-код-на-lisp.el --- Поддержка Lisp в Emacs + мудрость SICP/HTDP -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 2.1
;; Keywords: lisp, elisp, scheme, cl, geiser, sly, macro, sicp, htdp
;; URL: https://example.com/про-код-на-lisp
;;
;;; Commentary:
;;
;;  «Programs must be written for people to read, and only incidentally for
;;   machines to execute.»                                          — SICP
;;
;;  «The essence of Lisp is the fact that programs are data.  The rest is
;;   just parentheses.»                                              — McCarthy
;;
;;  Коан:
;;    Ученик: «Учитель, у меня лишняя закрывающая скобка. Что делать?»
;;    Мастер  : «Добавь ещё одну форму и назови её макросом».
;;
;;  Этот файл — попытка собрать лучшую лисп-практику из SICP, HTDP, «On Lisp»,
;;  koans и фольклора, упаковать её в Emacs-конфиг и остаться при этом
;;  минималистами.  Стараемся:
;;
;;  • Ничего не грузить зря  (use-package :defer t).
;;  • Избегать глобального мусора (имена на русском, чтоб не конфликтовать).
;;  • Давать REPL-опыт первого класса (eval, инспекция, helper-буферы).
;;  • Поддерживать сразу несколько диалектов (Elisp, Scheme, Common Lisp).
;;  • Добавлять крупицы мудрости в комментариях, чтобы конфиг читался как
;;    книга о Лиспе.
;;
;;  Структура:
;;   0.  Введение и зависимости.
;;   1.  Базовые eval-команды.
;;   2.  Emacs Lisp (биндинги, печать, оптимизации).
;;   3.  Отладка, инспекция, документация.
;;   4.  Форматирование.
;;   5.  REPL и диалекты (Scheme/CL).
;;   6.  Структурное редактирование и макросы.
;;   7.  «Мини-SICP» — полезные комбинаторы и примеры.
;;
;;  Подключайте через (require 'про-код-на-lisp) из init.el.
;;
;;; Code:

;;;; 0. Введение и зависимости
(require 'use-package)
(require 'установить-из)          ; локальный хелпер для straight/quelpa/…
(require 'cl-lib)                 ; cl-macros нужны «Мини-SICP»

;;;; 1. Базовые eval-команды
(defun выполнить-регион-или-буфер ()
  "Eval активный регион или весь буфер.
Философия REPL: изменения должны тестироваться мгновенно."
  (interactive)
  (if (use-region-p)
      (progn (eval-region (region-beginning) (region-end))
             (deactivate-mark))
    (перевыполнить-буфер)))

(defun перевыполнить-буфер ()
  "Eval всех верхнеуровневых форм в текущем буфере.
Идеально для org-babel/literate конфигов."
  (interactive)
  (unless (derived-mode-p 'emacs-lisp-mode 'lisp-interaction-mode)
    (user-error "перевыполнить-буфер: работает только в Emacs Lisp буфере"))
  (let ((orig-buf (current-buffer))
        pos msg)
    (condition-case err
        (save-excursion
          (check-parens))
      (scan-error
       (let* ((data (cdr err))
              (start (nth 1 data))
              (end   (nth 2 data)))
         (setq pos (or end start)
               msg (or (car data) (error-message-string err)))))
      (error
       (setq msg (error-message-string err))))
    (if msg
        (progn
          (when pos
            (with-current-buffer orig-buf
              (goto-char pos)
              (recenter)
              (when (fboundp 'pulse-momentary-highlight-one-line)
                (pulse-momentary-highlight-one-line pos))))
          (pop-to-buffer orig-buf)
          (message "Неправильные скобки: %s" (or msg "Unbalanced parentheses")))
      (save-excursion
        (eval-buffer)
        (message "Буфер выполнен заново!")))))

;;;; 2. Emacs Lisp: биндинги, печать, оптимизации
(use-package emacs
  :defer t
  :bind (:map emacs-lisp-mode-map
              ("C-c C-c" . выполнить-регион-или-буфер)
              ("C-x M-e" . eval-print-last-sexp)
              ("C-c C-k" . перевыполнить-буфер)))

(setq eval-expression-print-level 20   ; не усекать дип-структуры
      eval-expression-print-length 50)

;; Хвостовая рекурсия   (when you need loops but stay pure)
(use-package tco      :defer t :ensure t)

;; async/await в Elisp   («Расслабься и дождись промиса»)
(use-package async-await :defer t :ensure t)

;;;; 3. Отладка, инспекция, документация
;;  «Секрет Лиспа — в системе разработки»  — поэтому инструменты важны.

;;; 3.1 Helpful + elisp-demos  — describe-* на стероидах
(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
         ([remap describe-variable] . helpful-variable)
         ([remap describe-symbol]   . helpful-symbol)
         ([remap describe-command] . helpful-command))
  :custom (helpful-max-buffers 7))

(use-package elisp-demos
  :ensure t
  :after helpful
  :config (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;; 3.2 elisp-refs  — поиск где символ используется (grep без grep)
(use-package elisp-refs
  :defer t
  :ensure t
  :commands (elisp-refs-function elisp-refs-macro elisp-refs-special))

;;; 3.3 zeal-at-point / dash-docs  — оффлайн-доки (CLHS, R5RS…)
(use-package zeal-at-point
  :defer t
  :ensure t
  :commands zeal-at-point
  :custom (zeal-at-point-docset-alist '(("emacs-lisp-mode" . ("elisp" "clhs"))
                                        ("lisp-mode"       . ("clhs"))
                                        ("scheme-mode"     . ("r5rs" "guile")))))

(use-package dash-docs
  :defer t
  :ensure t)

;;; 3.4 Inspector (древо любого объекта)
(use-package inspector :defer t :ensure t)

;;; 3.5 Bug-hunter (бинд не ставим, чтобы не тормозило)
(use-package bug-hunter :defer t :ensure t)

;;; 3.6 Flymake для elisp
;; Базовая настройка Flymake для elisp + package-lint
(use-package fly
  :defer t
  :init (установить-из :repo "ROCKTAKEY/flymake-elisp-config")
  :functions (flymake-elisp-config-global-mode flymake-elisp-config-auto-mode)
  :config
  (flymake-elisp-config-global-mode)
  (flymake-elisp-config-auto-mode))

;; package-lint: проверка качества и метаданных пакетов
(use-package package-lint
  :defer t
  :ensure t
  :init
  ;; Подключаем package-lint как backend Flymake, если доступен.
  )n

;;;; 4. Форматирование
(use-package format-all
  :defer t
  :ensure t
  :hook (emacs-lisp-mode . format-all-mode))

;;;; 5. REPL и другие диалекты
;;; 5.1 Scheme → Geiser
(use-package geiser
  :defer t
  :ensure t
  :custom (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-mode-start-repl-p nil))

(use-package geiser-guile
  :defer t
  :ensure t
  :requires geiser
  :config
  (setq geiser-guile-manual-lookup-nodes '("guile" "guix"))
  (when (and (file-exists-p "shell.nix") (executable-find "nix-shell"))
    (setq geiser-guile-binary "nix-shell --run guile")))

;;; 5.2 Common Lisp → Sly
(use-package sly
  :defer t
  :ensure t
  :custom (sly-lisp-implementations '((sbcl ("sbcl") :coding-system utf-8-unix)))
  :hook (sly-mode . sly-editing-mode))

(use-package sly-quicklisp :after sly :ensure t)
(use-package sly-macrostep :after sly :ensure t)

;;;; 6. Структурное редактирование и макросы
;;  «Когда я вижу лишний слой скобок, я испытываю духовную радость» — неизвестный лиспер.

;; ;;; 6.1 Lispy  — редактируй s-expr как список, а не текст
;; (use-package lispy
;;   :ensure t
;;   :hook ((emacs-lisp-mode lisp-interaction-mode ielm-mode scheme-mode lisp-mode) . lispy-mode)
;;   :custom (lispy-teleport-global nil))

;;; 6.2 Paredit (иногда хочется классики)
(use-package paredit
  :ensure t
  :hook ((scheme-mode lisp-mode) . paredit-mode))

;;; 6.3 Lispyville — evil-keybindings поверх Lispy (опционально)
;; (use-package lispyville
;;   :after lispy
;;   :ensure t
;;   :hook (lispy-mode . lispyville-mode))

;;; 6.4 Rainbow-delimiters — уровень вложенности как радуга
(use-package rainbow-delimiters :ensure t :hook (prog-mode . rainbow-delimiters-mode))

;;; 6.5 Macrostep — пошаговый expand
(use-package macrostep
  :defer t
  :ensure t
  :bind (:map emacs-lisp-mode-map ("C-c >" . macrostep-expand)
              ("C-c <" . macrostep-collapse)
              :map lisp-interaction-mode-map ("C-c >" . macrostep-expand)
              ("C-c <" . macrostep-collapse)))

;;; 6.6 Eros — вывод результата прямо в буфере
(use-package eros  :defer t :ensure t :config (eros-mode 1))

;;; 6.7 russian-lisp-mode - перевод лиспа на русский язык

(use-package russian-lisp-mode
  :load-path "~/Code/russian-lisp-mode/")

;;; Полезные функции

(defun pro/check-parens-in-directory (dir)
  "Проверить все .el файлы в DIR с помощью `check-parens'.
Результат выводится в буфере *check-parens/."
  (interactive "DDirectory: ")
  (let ((files (directory-files-recursively dir "\\.el\\'"))
        (errs 0))
    (with-current-buffer (get-buffer-create "*check-parens/")
      (erase-buffer)
      (dolist (f files)
        (let ((buf (find-file-noselect f)))
          (unwind-protect
              (let (ok err-msg)
                (with-current-buffer buf
                  (condition-case e
                      (progn (check-parens)
                             (setq ok t))
                    (error
                     (setq ok nil
                           err-msg (error-message-string e)))))
                (when (not ok)
                  (setq errs (1+ errs)))
                (insert (format "%-3s %s\n" (if ok "OK" "ERR") f))
                (when (not ok)
                  (insert (format "    %s\n" err-msg))))
            (kill-buffer buf))))
      (insert (format "Summary: %d files, %d with errors\n" (length files) errs))
      (goto-char (point-min))
      (display-buffer (current-buffer)))
    (message "Done: %d files, %d with errors" (length files) errs)))

(defun pro/dired-check-parens-marked ()
  "Запустить `check-parens' для всех помеченных файлов в Dired.
Результат в буфере *check-parens/."
  (interactive)
  (require 'dired)
  (let* ((files (cl-remove-if #'file-directory-p (dired-get-marked-files)))
         (errs 0))
    (with-current-buffer (get-buffer-create "*check-parens/")
      (erase-buffer)
      (dolist (f files)
        (let ((buf (find-file-noselect f)))
          (unwind-protect
              (let (ok err-msg)
                (with-current-buffer buf
                  (condition-case e
                      (progn (check-parens)
                             (setq ok t))
                    (error
                     (setq ok nil
                           err-msg (error-message-string e)))))
                (when (not ok)
                  (setq errs (1+ errs)))
                (insert (format "%-3s %s\n" (if ok "OK" "ERR") f))
                (when (not ok)
                  (insert (format "    %s\n" err-msg))))
            (kill-buffer buf))))
      (insert (format "Summary: %d files, %d with errors\n" (length files) errs))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun pro/reload-all-elisp-in-dired-directory ()
  "Перезагрузить все файлы .el в текущей папке Dired."
  (interactive)
  (let ((dir (dired-current-directory)))
    (dolist (file (directory-files dir t "\\.el$"))
      (load (file-name-sans-extension file) nil 'nomessage))
    (message "Все .el-файлы из %s перезагружены." dir)))

;;   «Пишите код, чтобы его было легко читать; если вам скучно, вставьте шутку,
;;    но так, чтобы компилятору было всё равно» — мануфактура лиспа, 2023.

(provide 'про-код-на-lisp)
;;; про-код-на-lisp.el ends here
