;;; Helm
;;;; Базовая настройка HELM

(leaf helm
  :ensure t
  :custom (
           (helm-M-x-always-save-history t)
           (helm-M-x-fuzzy-match t)
           (helm-M-x-requires-pattern nil)
           (helm-adaptive-history-length 100)
           (helm-apropos-fuzzy-match t)           
           (helm-autoresize-max-height 35)
           (helm-autoresize-min-height 30)
           (helm-autoresize-mode t)
           (helm-bookmark-show-location t)           
           (helm-buffer-max-length 60)
           (helm-buffers-fuzzy-matching t)
           (helm-completion-in-region-fuzzy-match t)
           (helm-completion-style 'helm-fuzzy)           
           (helm-display-header-line nil) 
           (helm-etags-fuzzy-match t)
           (helm-ff-skip-boring-files t)
           (helm-imenu-fuzzy-match t)
           (helm-inherit-input-method nil)
           (helm-input-idle-delay 0.01)
           (helm-lisp-fuzzy-completion t)
           (helm-locate-fuzzy-match t)
           (helm-mode-fuzzy-match t)
           (helm-mode-handle-completion-in-region t)
           (helm-mode-line-string nil)
           (helm-quick-update t)
           (helm-recentf-fuzzy-match t)
           (helm-search-suggest-action-wikipedia-url "https://ru.wikipedia.org/wiki/Special:Search?search=%s")           
           (helm-semantic-fuzzy-match t)
           (helm-session-fuzzy-match t)
           (helm-split-window-default-side "below")
           (helm-split-window-in-side-p t)           
           (helm-split-window-inside-p t)
           (helm-wikipedia-suggest-url "https://ru.wikipedia.org/w/api.php?action=opensearch&search=%s")
           (helm-wikipedia-summary-url "https://ru.wikipedia.org/w/api.php?action=parse&format=json&prop=text&section=0&page=%s")
           (helm-yas-display-key-on-candidate t)           
           ; (helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
           (helm-idle-delay 0.0)   
           )
  :bind (("M-x" . helm-M-x)
         ("s-x" . helm-M-x) ;; macos style
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x y" . helm-show-kill-ring)
         ("C-x C-y" . helm-show-kill-ring)
         ("C-x m" . helm-all-mark-rings)
         ("C-x f" . helm-recentf)
         ("<f1> a" . helm-apropos)
         ("C-c sw" . helm-wikipedia-suggest)
         ("C-c i" . helm-imenu)
         ("M-m" . helm-imenu)
         :map helm-map
         ("s-<tab>" . helm-next-line)
         ("<s-iso-lefttab>" . helm-previous-line)
         :map dired-mode-map
         ("s" . helm-do-grep-ag)
         ("f" . helm-find)
         ("M-+" . helm-find-files)
         )

  :hook ((helm-major-mode . my/helm-fonts))
  :init  
  ;; (set-face-attribute 'helm-source-header
  ;;                     nil
  ;;                     :foreground (face-attribute 'helm-selection :background)
  ;;                     :background (face-attribute 'helm-selection :foreground1)                          
  ;;                     :box t
  ;;                     :height 1.0)

  (defun helm-display-mode-line (source &optional force)
    (setq mode-line-format nil))
  
  (require 'helm-config)

  (helm-adaptive-mode t)
  (helm-autoresize-mode 1)
  (helm-mode t)
  :config
  ;; TODO after exwm
  )

;;;; Шрифт для буфера Helm

(require 'face-remap)

(defun my/helm-fonts () (interactive)
       ;; TODO: if window-system
       ;; (progn 
       ;;   (cond ((< (window-width) 50) (face-remap-add-relative 'default :height 0.55))
       ;;         ((< (window-width) 70) (face-remap-add-relative 'default :height 0.68))
       ;;         ((< (window-width) 120) (face-remap-add-relative 'default :height 0.75))
       ;;         ((> (window-width) 120) (face-remap-add-relative 'default :height 0.8))
       ;;         )
         
       ;;   (variable-pitch-mode -1))
       )

;;;; Ускорение

; [[https://github.com/emacs-helm/helm/issues/1976][Говорят]], нижеследующее ускоряет /Helm/ в EMACS 26+:

(setq-default x-wait-for-event-timeout nil)

;;;; Поиск строки во всех буферах

(leaf helm-swoop
  :ensure t  
  :bind (("M-s" . nil)
         ("C-M-s" . helm-swoop-from-isearch)
         ("M-s" . helm-swoop)
         ("s-s" . helm-multi-swoop-all)
         :map isearch-mode-map
         ("M-s" . helm-swoop-from-isearch))
  :custom (
           (helm-swoop-pre-input-function (lambda () ""))))

;;;; Список команд мажорного мода  M-S-X (C-M-x)

(leaf helm-smex
  :ensure t    
  :bind (("M-X" . helm-smex-major-mode-commands) 
         ("C-M-x" . helm-smex-major-mode-commands)
         )
  :custom ((helm-smex-show-bindings t))
  :config
  (global-set-key [remap execute-extended-command] #'helm-smex))

;;;; Список биндингов

(leaf helm-descbinds
  :ensure t
  :bind (("<f1> w" . helm-descbinds)
         ("<f1> b" . helm-descbinds)))


;;;; Быстрый поиск строки по файлам рекурсивно

;; (leaf helm-ls-hg
;;   :bind (("C-c hgf" . helm-hg-find-files-in-project)))

(leaf helm-ag  
  :bind (("C-c sa" . helm-do-grep-ag) )
  :custom ( (helm-ag-fuzzy-match t)
            (helm-ag-use-agignore t)
            )
  :ensure t)

;;; HELM для exwm

(leaf helm-exwm
  :ensure t
  :if window-system
  :after (exwm)
  :config
  (exwm-input-set-key (kbd "s-<tab>") 'helm-exwm)
  (exwm-input-set-key (kbd "s-x") 'helm-run-external-command)    
  (exwm-input-set-key (kbd "s-b") 'helm-exwm)
  (exwm-input-set-key (kbd "C-c b") 'helm-mini)  
  )

;;; Helm + lsp

(leaf helm-lsp :commands helm-lsp-workspace-symbol)

;;; Helm FLX
  
; (leaf helm-flx
;   :ensure t
;   :init
;   (helm-flx-mode t)
;   )

;;; Helm dash

; (leaf helm-dash
;   :ensure t
;   :init
;   (setq helm-dash-browser-func 'eww))

;;; Helm posframe

(leaf helm-posframe
  :ensure t
  :custom ((helm-posframe-border-width 1) 
           (helm-posframe-font "Fira Code")
           (helm-posframe-parameters '((parent-frame nil)))
           )
  :init
  (setq helm-posframe-parameters '((parent-frame nil)))
  ;; (setq helm-display-function #'helm-default-display-buffer) ;helm-display-buffer-in-own-frame)
  ;; (helm-posframe-enable)
  )

;; (defun my-helm-display-frame-center (buffer &optional resume)
;;   "Display `helm-buffer' in a separate frame which centered in
;; parent frame."
;;   (if (not (display-graphic-p))
;;       ;; Fallback to default when frames are not usable.
;;       (helm-default-display-buffer buffer)
;;     (setq helm--buffer-in-new-frame-p t)
;;     (let* ((parent (selected-frame))
;;            (frame-pos (frame-position parent))
;;            (parent-left (car frame-pos))
;;            (parent-top (cdr frame-pos))
;;            (width (/ (frame-width parent) 2))
;;            (height (/ (frame-height parent) 3))
;;            tab-bar-mode
;;            (default-frame-alist
;;              (if resume
;;                  (buffer-local-value 'helm--last-frame-parameters
;;                                      (get-buffer buffer))
;;                `((parent . ,parent)
;;                  (width . ,width)
;;                  (height . ,height)
;;                  (undecorated . ,helm-use-undecorated-frame-option)
;;                  (left-fringe . 0)
;;                  (right-fringe . 0)
;;                  (tool-bar-lines . 0)
;;                  (line-spacing . 0)
;;                  (desktop-dont-save . t)
;;                  (no-special-glyphs . t)                 
;;                  (skip-taskbar . t)
;;                  (undecorated . t)
;;                  (inhibit-double-buffering . t)
;;                  (tool-bar-lines . 0)
;;                  (left . ,(+ parent-left (/ (* (frame-char-width parent) (frame-width parent)) 4)))
;;                  (top . ,(+ parent-top (/ (* (frame-char-width parent) (frame-height parent)) 6)))
;;                  (title . "Helm")
;;                  (vertical-scroll-bars . nil)
;;                  (menu-bar-lines . 0)
;;                  (fullscreen . nil)
;;                  (visible . ,(null helm-display-buffer-reuse-frame))
;;                  ;; (internal-border-width . ,(if IS-MAC 1 0))
;;                 )))
;;            display-buffer-alist)
;;       (set-face-background 'internal-border (face-foreground 'default))
;;       (helm-display-buffer-popup-frame buffer default-frame-alist))
;;     (helm-log-run-hook 'helm-window-configuration-hook)))

;; (setq helm-display-function 'my-helm-display-frame-center)

;;; Helm tree sitter

(leaf helm-tree-sitter
  :ensure t
    :bind (("s-m" . helm-tree-sitter))

    )

(leaf helm-tramp  
  :ensure t  
  )

(leaf helm-projectile
  :after projectile
  :ensure t)

;;; dobro-helm

(provide 'dobro-general-completion-helm)


