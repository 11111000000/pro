;;; про-внешний-вид.el --- Внешний вид и интерфейс Emacs -*- lexical-binding: t -*-
;;; Commentary:
;; Этот модуль оформляет внешний вид Emacs: интерфейс, иконки, курсоры, вкладки и качественные мелочи,
;; чтобы работа с Emacs радовала глаз и ощущалась современно.
;;
;; Здесь вы найдёте:
;; — Минималистичный интерфейс (скрытие лишнего, плавность, отключение сигналов)
;; — Улучшенный минибуфер, часы/батарея
;; — Надёжная иконография (kind-icon, nerd-icons, dired, ibuffer и др.)
;; — Кастомизация курсора и прокрутки
;; — Красивые вкладки pro-tabs
;; — Ещё много полезного!

;;; Code:

(require 'установить-из)      ;; Универсальный способ подключения внешних пакетов из нашего ecospace.

;;;; 1. Базовый вид Emacs (гигиена интерфейса)

;; Отключаем фоновые раздражители: звуковые/визуальные сигналы.
(setq visible-bell nil
      ring-bell-function 'ignore)

;; Без лишнего мусора: убираем scroll-bar, panel, menu.
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
;; tool-bar-mode и menu-bar-mode настраиваются в early-init.el через default-frame-alist.

;;;; 2. Разделители окон (горизонтальные линии для визуального комфорта)

(when window-system
  (setq window-divider-default-bottom-width 1
        window-divider-default-places 'bottom-only)
  (window-divider-mode 1))

;;;; 3. Минибуфер — продвинутый центр внимания

(setq-default
 enable-recursive-minibuffers t           ; Рекурсивные минибуферы для цепочек команд.
 max-mini-window-height 0.25              ; До 25% экрана — места хватит!
 message-truncate-lines nil)              ; Не урезать длинные сообщения.
(setq resize-mini-windows t)              ; Минибуфер автоматически меняет высоту.

;; Украшаем минибуфер: элегантная строка состояния (shaoline) с иконками.
(use-package shaoline
  :if window-system
  :after (all-the-icons)
  :functions (shaoline-mode)
  :init
  (установить-из :repo "11111000000/shaoline")
  :custom (shaoline-right-padding 12)
  :config (shaoline-mode t))

;; Показывать батарею и часы, чтобы не терять связь с реальностью вне Emacs.
(require 'time)
(display-battery-mode 1)
(display-time-mode 1)

;;;; 4. Иконки повсюду: эстетика современного интерфейса

;; --- (Опционально) Подключение глобального набора иконок All-the-icons.
;; (use-package all-the-icons
;;   :ensure t
;;   :if window-system
;;   :custom
;;   (all-the-icons-scale-factor 1)
;;   (all-the-icons-default-adjust 0))

;; Иконки для автодополнения через kind-icon — прямо в выпадающем меню corfu.
(use-package kind-icon
  :ensure t
  :after corfu
  :defines (corfu-margin-formatters)
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default)
  :config
  ;; Встраиваем иконки удобно сбоку от каждого варианта.
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  ;; При смене темы сбрасываем кэш иконок (иначе цвета иногда не обновляются).
  (add-hook 'kb/themes-hooks #'(lambda () (kind-icon-reset-cache))))

;; Иконки в подсказках (M-x, файлы, команды) через nerd-icons-completion + marginalia.
(use-package nerd-icons-completion
  :ensure t
  :functions (nerd-icons-completion-mode)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (unless (display-graphic-p)
    (nerd-icons-completion-mode))) ; В терминале не мучаемся иконками.

;; Иконки в dired через treemacs — файловый менеджер тоже достоин красоты.
(use-package treemacs-icons-dired
  :ensure t
  :functions (treemacs-icons-dired-mode)
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :init
  (add-hook 'after-load-theme-hook
            (lambda ()
              ;; После смены темы перезапускаем отображение иконок.
              (treemacs-icons-dired-mode -1)
              (sleep-for 0)
              (treemacs-icons-dired-mode 1))))

;; Иконки в ibuffer (мощный переключатель буферов).
(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

;;;; 5. Хуки после смены темы: чтобы все визуальные навороты поймали свежий вид

(defvar after-load-theme-hook nil
  "Хук, срабатывающий после установки (load-theme) новой темы.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Запускать after-load-theme-hook после каждой загрузки темы."
  (run-hooks 'after-load-theme-hook))

;;;; 6. Курсор — выразительный и информативный

(setq cursor-type '(bar . 3)
      x-stretch-cursor t)     ; Курсор по высоте буквы, не точки.
(setq-default cursor-in-non-selected-windows t)  ; В невыбранных окнах — полупрозрачный.

;; Усиленный контроль над типами и цветами курсора в зависимости от input method.
(use-package cursor-chg
  :init (установить-из :repo "emacsmirror/cursor-chg")
  :functions (change-cursor-mode)
  :config
  (setq-default curchg-input-method-cursor-color "orange"
                curchg-default-cursor-type '(bar . 3)
                curchg-default-cursor-color "forest green"
                curchg-change-cursor-on-input-method-flag t)
  (change-cursor-mode t))

;;;; 7. Прокрутка: минимализм и отзывчивость

(setq-default
 scroll-conservatively 80
 scroll-step 1
 scroll-margin 5
 hscroll-step 1
 auto-window-vscroll nil
 fast-but-imprecise-scrolling t
 jit-lock-defer-time 0
 hscroll-margin 1)

;; Плавная внутренняя прокрутка изображений, org, markdown и web-рендеров.
(use-package iscroll
  :ensure t
  :functions (iscroll-mode)
  :hook ((org-mode markdown-mode image-mode eww-mode w3m-mode) . iscroll-mode))

;;;; 8. imenu — мгновенное меню оглавления текущего файла (например, для org)

(use-package imenu
  :custom ((imenu-auto-recsan t)))  ;; Автоматически пересканировать большие файлы.

;;;; 9. Диалоги — без занудного yes/no, только y/n!

(defalias 'yes-or-no-p 'y-or-n-p)

;;;; 10. Работа с изображениями: интерактивное масштабирование и просмотр

(require 'image-mode)
(use-package image+
  :defer t
  :ensure t
  :hook (image-mode . image+)
  :bind ((:map image-mode-map
               ("0" . imagex-sticky-restore-original)
               ("+" . imagex-sticky-maximize)
               ("=" . imagex-sticky-zoom-in)
               ("-" . imagex-sticky-zoom-out))))

;;;; 11. Красота деталей: фринжи (fringes), разделители и утилиты

;; Эстетика для fringes: тонкие цветные индикаторы, направляющие для split-окон.
(use-package modern-fringes
  :ensure t
  :config (modern-fringes-mode t))

;; Быстрые prettify utils. Используется во многих красивых режимах.
(use-package prettify-utils
  :init (установить-из :repo "Ilazki/prettify-utils.el"))

;;;; 12. Вкладки: современный tab-bar и tab-line
;;;;    (pro-tabs — сразу поддерживает и то, и другое)

(defun pro/tab-line-close-tab ()
  "Закрыть текущую вкладку tab-line корректно:
Если вкладок несколько — просто kill-buffer;
если осталась последняя — и закрыть окно."
  (interactive)
  (let ((win (selected-window)))
    (if (> (length (tab-line-tabs-window-buffers)) 1)
        (kill-buffer (window-buffer win))
      (progn
        (kill-buffer (window-buffer win))
        (when (window-live-p win)
          (delete-window win))))))

(require 'seq)

(use-package pro-tabs
  :init (установить-из :repo "11111000000/pro-tabs")
  :commands (pro-tabs-mode pro-tabs-open-new-tab pro-tabs-close-tab-and-buffer)
  :bind (
         ;; Быстрое управление tab-bar (глобально и в режимах)
         ("s-n" . tab-bar-switch-to-next-tab)
         ("s-p" . tab-bar-switch-to-prev-tab)
         ("s-w" . tab-bar-close-tab)
         :map tab-bar-mode-map
         ("s-n" . tab-bar-switch-to-next-tab)
         ("s-p" . tab-bar-switch-to-prev-tab)
         ("s-<tab>" . tab-bar-switch-to-next-tab)
         ("S-s-<iso-lefttab>" . tab-bar-switch-to-prev-tab)
         ("s-w" . tab-bar-close-tab)
         :map tab-line-mode-map
         ("s-n" . tab-line-switch-to-next-tab)
         ("s-p" . tab-line-switch-to-prev-tab)
         ("s-w" . pro/tab-line-close-tab))
  :custom
  (pro-tabs-enable-icons t)
  ;; (pro-tabs-max-tab-name-length 25)
  ;; (pro-tabs-tab-bar-height 18)
  ;; (pro-tabs-tab-line-height 20)
  :config
  (pro-tabs-mode 1))

;;;; 13. Дополнительное (комментированный код — для экспериментов)

;; Подсветка строки в текущем окне (включайте если нужно)
;; (defun hl-line-only-in-current-window ()
;;   ;; ...см. предыдущую версию...
;; )
;; (add-hook 'window-selection-change-functions hl-line-only-in-current-window)

;; Альтернативная подсветка перемещения (pulse)
;; (use-package pulsar
;;   :disabled t
;;   :ensure t
;;   :hook ((next-error xref-after-return) . pulsar-pulse-line)
;;   :custom (pulsar-face 'pulsar-green)
;;   :config (pulsar-global-mode -1))

;; Цветной фон для служебных буферов (solaire-mode)
;; (use-package solaire-mode
;;   :ensure t
;;   :config (solaire-global-mode t))

;; Поддержка выделения активного окна
;; (use-package selected-window-accent-mode
;;   :config (selected-window-accent-mode 1)
;;   :custom
;;   (selected-window-accent-fringe-thickness 10)
;;   (selected-window-accent-custom-color nil)
;;   (selected-window-accent-mode-style 'subtle))

;;;; 14. Финал

(setq-default confirm-kill-processes nil)   ;; Не спрашивать подтверждение на kill процессов

(provide 'про-внешний-вид)
;;; про-внешний-вид.el ends here
