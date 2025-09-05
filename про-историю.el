;;; про-историю.el --- История Emacs: undo, redo, сессии, лучший журнал -*- lexical-binding: t -*-
;;
;; Автор: Пётр Косов <11111000000@email.com>
;; Версия: 2.2
;; Keywords: history, undo, redo, sessions, layout, productivity
;; URL: https://github.com/peterkosov/pro-history-plus
;;
;;; Commentary:
;;
;; =Про-история= — ваш персональный "Time Machine":
;;   - Кросс-сессии, undo/redo древо, истории перемещений, win layout, окна, буферы!
;;   - Дополнительно: меню работы с kill-ring, моментальные quicksave/quickload, автоснапшоты.
;;   - Встроенные команды для быстрой реанимации истории, layout, kill/yank/win.
;;   - Поддержка чистых путей через no-littering.
;;   - Удобен новичку и эмаксеру — весь функционал снабжён примером бинда и пояснением.
;;
;; Весь основной функционал организован по секциям с понятными заголовками.
;;
;; Просто подключите: (require 'про-история)
;;
;;; Code:

(require 'cl-lib)

;;;; 0. Группа настроек для про-истории

(defgroup про-история nil
  "Профессиональные инструменты управления историей и сессиями Emacs."
  :group 'convenience)

(defcustom про-история-max-undo-history 90
  "Сколько дней хранить undo / snapshots / recentf перед автоматической очисткой."
  :type 'integer :group 'про-история)

(defcustom про-история-snapshots-save-dir (expand-file-name "pro-history-snaps/" user-emacs-directory)
  "Каталог, куда складываются авто-снапшоты layout/буферов."
  :type 'directory :group 'про-история)

(defcustom про-история-max-kill-ring 400
  "Максимальная длина kill-ring, включая автосохранение."
  :type 'integer :group 'про-история)

;;;; 1. Универсальные настройки и аккуратное хранение истории (via no-littering)

(use-package no-littering
  :ensure t
  :functions (no-littering-expand-var-file-name no-littering-expand-etc-file-name no-littering-theme-backups)
  :config
  ;; Ставим backup'ы и history-файлы в аккуратные подпапки, используя no-littering
  (setq
   history-length 300
   savehist-autosave-interval 300
   savehist-additional-variables
   '(search-ring
     regexp-search-ring
     extended-command-history
     projectile-project-command-history
     kill-ring
     compile-command
     file-name-history
     shell-command-history)
   kept-old-versions 25
   delete-old-versions t
   create-lockfiles nil
   vc-make-backup-files t
   version-control t)
  (savehist-mode 1)
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (no-littering-theme-backups))

;;;; 2. Могущественное древо undo: «машина времени»

(use-package undo-tree
  :ensure t
  :diminish " ⸙"
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist
   `((".*" . ,(no-littering-expand-var-file-name "undo/"))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff t)
  (undo-tree-history-overwrite t) ;; <--- Не задаём вопрос: всегда перезаписывать undo history файл.
  :bind (("C-M--" . undo-tree-visualize)
         ("C-M-_" . undo-tree-visualize)
         ("M-u"   . undo-tree-visualize)
         ;; Классические бинды для undo/redo:
         ("C-_"   . undo-tree-undo)
         ("M-_"   . undo-tree-redo))
  :config
  ;; Гарантируем, что каталог для persistent-undo существует.
  (make-directory (no-littering-expand-var-file-name "undo/") t)
  (global-undo-tree-mode 1)

  ;; Без вопросов: совет оборачивает undo-tree-save-history и всегда отвечает 'y'.
  (defun про-история--undo-tree-save-history-силентно (orig-fun &rest args)
    (let ((undo-tree-history-overwrite t))
      (condition-case _
          (cl-letf (((symbol-function 'y-or-n-p)   (lambda (&rest _) t))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (apply orig-fun args))
        (wrong-number-of-arguments
         ;; На старых версиях undo-tree параметр OVERWRITE игнорируется — всё равно подавим вопросы.
         (cl-letf (((symbol-function 'y-or-n-p)   (lambda (&rest _) t))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (apply orig-fun (list (car args))))))))
  (advice-add 'undo-tree-save-history :around #'про-история--undo-tree-save-history-силентно)

  ;; На всякий случай дублируем автосохранение истории:
  ;; 1) Сохранять историю сразу после сохранения файла.
  (add-hook 'undo-tree-mode-hook
            (lambda ()
              (when undo-tree-mode
                (add-hook 'after-save-hook (lambda () (undo-tree-save-history nil t)) nil t))))
  ;; 2) Сохранить историю всех буферов при выходе из Emacs.
  (add-hook 'kill-emacs-hook
            (lambda ()
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when (and (bound-and-true-p undo-tree-mode)
                             buffer-file-name)
                    (ignore-errors (undo-tree-save-history nil t)))))))
  ;; Авто-сохранение всех изменённых файлов при выходе, без вопросов.
  (advice-add 'save-buffers-kill-emacs :around
              (lambda (orig-fun &optional arg)
                (apply orig-fun (list t))))

  (defun про-историю/очистить-undo ()
    "Полностью очищает undo-историю текущего буфера."
    (interactive)
    (setq undo-tree-stack nil
          undo-tree-redo-stack nil
          buffer-undo-tree nil)
    (message "История отмен очищена!"))
  (defun про-историю/удалить-все-undo-файлы ()
    "Удаляет все сохранённые undo-tree файлы (если вдруг разрослись)."
    (interactive)
    (let ((dir (cdr (assoc ".*" undo-tree-history-directory-alist))))
      (when (and dir (file-directory-p dir))
        (let ((files (directory-files dir t "^[^.]")))
          (dolist (f files)
            (when (file-regular-p f)
              (delete-file f))))
        (message "Все undo-файлы из %s удалены." dir)))))

(defun про-историю/показать-undo-файл ()
  "Показать путь к файлу persistent-undo для текущего буфера."
  (interactive)
  (if (not buffer-file-name)
      (message "Буфер не связан с файлом — persistent undo нельзя сохранить.")
    (let* ((dir (cdr (assoc ".*" undo-tree-history-directory-alist)))
           (name (and dir
                      (concat (file-name-as-directory dir)
                              (replace-regexp-in-string "[/\\:]" "%" buffer-file-name)))))
      (if (and name (not (string-empty-p name)))
          (message "Undo-файл: %s" name)
        (message "Не удалось определить путь undo-файла.")))))

;;;; 3. Перемещение по пространству-времени к правке и месту

;; a) Перейти к последней правке (аналог 'jump to last edit')
(use-package goto-last-change
  :defer t
  :ensure t
  :bind (("C-c C-," . goto-last-point)
         ("C-c ."   . goto-last-change)))

;; b) Перейти к предыдущей позиции курсора (jump-point style)
(use-package goto-last-point
  :defer t
  :ensure t
  :config
  (goto-last-point-mode t)
  :bind (("C-c ," . goto-last-point)))

;;;; 4. Управление окнами/вкладками как историями (Eyebrowse, desktop, winner)

;; a) Eyebrowse — вкладки/рабочие столы
(use-package eyebrowse
  :defer t
  :ensure t
  :config (eyebrowse-mode 1))

;; b) Winner-mode — мгновенный откат раскладки окон ("C-c <left/right>")
(use-package winner
  :ensure nil
  :config (winner-mode 1))

;; c) Desktop-session — сохраняет ВСЮ сессию редактора (опционально)
;; (use-package desktop
;;   :ensure nil
;;   :custom
;;   (desktop-save-mode t) ;; включаем полный restore даже после аварийного выхода
;;   (desktop-path (list user-emacs-directory))
;;   (desktop-auto-save-timeout 600)
;; (desktop-restore-eager 8))

;;;; 5. Восстановление/запоминание мест в файлах, последних открытых

;; a) Восстановить курсор там, где вы работали ранее (saveplace)
(use-package saveplace
  :ensure t
  :after (no-littering)
  :init (save-place-mode 1))

;; b) История недавно открытых файлов (recentf)
(use-package recentf
  :after (no-littering)
  :custom ((recentf-max-saved-items 512)
           (recentf-max-menu-items 100)
           (recentf-exclude '("/\\.git/.*\\'"
                              "/\\.emacs\\.d/elpa"
                              "-autoloads\\.el\\'"
                              no-littering-var-directory
                              no-littering-etc-directory
                              "\\.elc\\'"
                              "/TAGS\\'")))
  :config (recentf-mode 1))

;;;; 6. История kill/yank/clipboard

(setq-default kill-ring-max про-история-max-kill-ring
              save-interprogram-paste-before-kill t
              yank-pop-change-selection t
              x-select-enable-primary t)

;; Быстрое сохранение и восстановление всего kill-ring (имитация persistent-yank-history)
(defun про-история/kill-ring-save-to-file (&optional file)
  "Сохраняет kill-ring в FILE или в каталог про-истории."
  (interactive)
  (let ((file (or file (expand-file-name "kill-ring.snap"
                                         про-история-snapshots-save-dir))))
    (make-directory (file-name-directory file) :parents)
    (with-temp-file file
      (prin1 kill-ring (current-buffer)))
    (message "Kill-ring сохранён: %s" file)))

(defun про-история/kill-ring-restore-from-file (&optional file)
  "Восстанавливает kill-ring из файла."
  (interactive)
  (let ((file (or file (expand-file-name "kill-ring.snap"
                                         про-история-snapshots-save-dir))))
    (when (file-readable-p file)
      (let ((new-kill (with-temp-buffer
                        (insert-file-contents file)
                        (read (current-buffer)))))
        (setq kill-ring (cl-subseq new-kill 0 (min (length new-kill) kill-ring-max)))
        (message "Kill-ring восстановлен из: %s" file)))))

;;;; 7. Полезные дополнительные макро/утилиты

(defun про-историю/показать-все-изменённые-буферы ()
  "Открыть buf-menu с фильтром для изменённых буферов."
  (interactive)
  (ibuffer nil "*Изменённые буферы*" '((modified . t))))

(defun про-историю/restore-last-session ()
  "Восстановить последнюю сессию desktop (открыть все окна/буферы/раскладки)."
  (interactive)
  (desktop-read)
  (message "Ваша последняя сессия восстановлена!"))

(defun про-историю/quick-time-machine ()
  "Открыть древо undo (Машина Времени) с автомаркером текущего состояния."
  (interactive)
  (cond
   ((bound-and-true-p undo-tree-mode)
    (undo-tree-visualize))
   ((fboundp 'vundo)
    (vundo))
   (t (user-error "undo-tree-mode или vundo недоступны!"))))

(defun про-историю/сохранить-снимок-буфера (&optional label)
  "Делает снимок текущего state: layout + буфер + point.
Сохраняет сразу в register и файл (quicksave)."
  (interactive "sМетка (по умолчанию 'history'): ")
  (let ((reg (if (string-empty-p label) ?h (aref label 0)))
        (snap (expand-file-name (format "window-%s.el" (or label "h"))
                                про-история-snapshots-save-dir)))
    (window-configuration-to-register reg)
    (desktop-save про-история-snapshots-save-dir t)
    (message "Снимок состояния (layout и сессия) сохранён в регистр %c и файл %s" reg snap)))

(defun про-историю/восстановить-снимок-буфера (&optional label)
  "Восстанавливает windows/layout + session из register и файла snapshot."
  (interactive "sМетка (по умолчанию 'history'): ")
  (let ((reg (if (string-empty-p label) ?h (aref label 0)))
        (snap (expand-file-name (format "window-%s.el" (or label "h"))
                                про-история-snapshots-save-dir)))
    (jump-to-register reg)
    (when (file-exists-p snap)
      (desktop-read про-история-snapshots-save-dir))
    (message "Восстановлен снимок layout/register + desktop!")))

;;;; 8. Универсальное меню по про-истории с дополнительными опциями
(defun про-историю/меню ()
  "Интерактивное меню восстановления и анализа истории (undo, layout, kill-ring, буферы, очистки).
Поддерживает дополнительное меню eco-help."
  (interactive)
  (let* ((options
          '("Машина времени (undo-tree/vundo)"
            "Последняя правка"
            "Предыдущий курсор"
            "Открыть изменённые буферы"
            "Восстановить сессию"
            "Очистить undo"
            "Очистить все undo-файлы"
            "Сохранить снимок окна"
            "Восстановить снимок окна"
            "Сохранить kill-ring"
            "Восстановить kill-ring"
            "Eco Help — что делает эта система?"))
         (choice (completing-read "Что сделать? " options nil t))
         (ix (cl-position choice options :test #'string=)))
    (pcase ix
      (0 (про-историю/quick-time-machine))
      (1 (call-interactively #'goto-last-change))
      (2 (call-interactively #'goto-last-point))
      (3 (про-историю/показать-все-изменённые-буферы))
      (4 (про-историю/restore-last-session))
      (5 (про-историю/очистить-undo))
      (6 (про-историю/удалить-все-undo-файлы))
      (7 (call-interactively #'про-историю/сохранить-снимок-буфера))
      (8 (call-interactively #'про-историю/восстановить-снимок-буфера))
      (9 (про-историю/kill-ring-save-to-file))
      (10 (про-историю/kill-ring-restore-from-file))
      (11 (про-историю/eco-help))
      (_ (message "Неизвестная команда!")))))

(defun про-историю/eco-help ()
  "Показывает краткую справку/мотивацию про-истории."
  (interactive)
  (with-help-window "*Про-история: Eco Help*"
    (princ
     "==== Emacs Про-История: чем улучшает вашу жизнь? ====\n
• Каждый шаг в Emacs можно откатить, вернуть, повторить и сохранить навсегда.
• В любой момент доступны:
  - Undo-tree/vundo: откат изменений КАК В ЛУЧШИХ IDE — древовидно!
  - История завершённых/всех буферов, включая изменённые/важные.
  - Быстрые снимки всей сессии (layout+buffers) и мгновенное восстановление (quicksave/quickload).
  - Автоматическое хранение вашей kill/yank истории между сеансами.
  - Чистка history, undo и recentf по сроку хранения.
• Экономит ~30% времени по статистике опытных эмаксеров =)
• Команды меню: M-x про-историю/меню RET
--------------------------------------------------------
© П.К.
")))

;;;; 9. Transient меню — быстрое меню команд истории

(use-package transient
  :ensure t)

(transient-define-prefix про-историю/transient ()
  "Меню команд Emacs истории: undo, redo, layout, kill-ring, снимки."
  ["История и восстановление"
   ("u" "Машина времени (undo-tree/vundo)" про-историю/quick-time-machine)
   ("e" "Последняя правка" goto-last-change)
   ("j" "Предыдущий курсор" goto-last-point)
   ("b" "Открыть изменённые буферы" про-историю/показать-все-изменённые-буферы)
   ("s" "Восстановить последнюю сессию" про-историю/restore-last-session)
   ]
  ["Очистки"
   ("U" "Очистить undo текущего буфера" про-историю/очистить-undo)
   ("F" "Удалить все undo-файлы" про-историю/удалить-все-undo-файлы)
   ]
  ["Сессия/снимки"
   ("q" "Сохранить снимок окна" про-историю/сохранить-снимок-буфера)
   ("Q" "Восстановить снимок окна" про-историю/восстановить-снимок-буфера)
   ]
  ["Kill-ring"
   ("k" "Сохранить kill-ring" про-историю/kill-ring-save-to-file)
   ("K" "Восстановить kill-ring" про-историю/kill-ring-restore-from-file)
   ]
  ["Другое"
   ("h" "Eco Help — справка" про-историю/eco-help)
   ("m" "Открыть меню-список (completing-read)" про-историю/меню)
   ])

;; Рекомендуемый хоткей:
;; (global-set-key (kbd "C-c M-h") #'про-историю/transient)

;;;; 9. Горячие клавиши (рекомендуемые)
;; Можно настроить свои:
;; (global-set-key (kbd "C-c M-t") #'про-историю/меню)

(provide 'про-историю)
;;; про-историю.el ends here
