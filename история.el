;; * История
;; ** Сохранение истории 

;; Сохранять действия в ~~/.emacs.d/history~

(setq-default history-delete-duplicates t
              history-length 300
              savehist-autosave-interval 300
              savehist-file "~/.emacs.d/history")

(savehist-mode t)

;; ** Текстовая Машина Времени

;; Дерево версий текста

(use-package undo-tree
  :ensure t
  :diminish " ⸙"
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `((".*" . ,(expand-file-name "~/.emacs.d/undo/"))))
  (undo-tree-visualizer-timestamps t)
  (undo-tree-visualizer-diff nil)

  :bind (("C-M--" . undo-tree-visualize)
         ("C-M-_" . undo-tree-visualize)
         ("M-u" . undo-tree-visualize))
  :init (global-undo-tree-mode 1))


;; Вернуться к последней правке

(use-package goto-last-change :ensure t
  :ensure t
  :bind (("C-c C-," . goto-last-point)))

;; Вернуться к предыдущей позиции курсора

(use-package goto-last-point 
  :ensure t
  :bind (("C-c ," . goto-last-point))
  :config
  (goto-last-point-mode t)
  )

;; (use-package backward-forward
;;   :ensure t
;;   :bind
;;   ("C-," . backward-forward-previous-location)
;;   ("C-." . backward-forward-next-location)
;;   :custom
;;   (mark-ring-max 60)
;;   (set-mark-command-repeat-pop t)
;;   :config
;;   (backward-forward-mode t))

;; ** История окон

;; Путешествие по истории окон - <C-c Left> / <C-c Right>

(use-package winner  
  :bind (("<XF86Back>" . winner-undo)
         ("<XF86Forward>" . winner-redo)
         ("s-u" . winner-undo))
  :init
  (winner-mode 1))

;; ** Память места

(use-package saveplace
  :ensure t
  :custom (save-place-file "~/.emacs.d/places")
  :init
  (save-place-mode t))

;; ** Недавние файлы

(use-package recentf  
  :custom ((recentf-max-saved-items 512)
           (recentf-max-menu-items 100)
           (recentf-exclude '("/\\.git/.*\\'"      ; Git contents
                              "/\\.emacs\\.d/elpa" ; ELPA
                              "/\\.emacs\\.d/etc/"
                              "/\\.emacs\\.d/var/"
                              "-autoloads\\.el\\'"
                              "\\.elc\\'"
                              "/TAGS\\'")))
  :config 
  (recentf-mode t))

;; ** История копирования

(setq-default kill-ring-max 300                
              save-interprogram-paste-before-kill t)

;; ** Бэкапы и временные файлы

(use-package no-littering
  :ensure t
  :custom ((make-backup-files t)
           (delete-by-moving-to-trash t)
           (backup-by-copying t)
           (kept-new-versions 25)
           (kept-old-versions 25)
           (delete-old-versions t)                                         
           (create-lockfiles nil)
           (vc-make-backup-files t)
           (version-control t))
  :config    
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)  
  (setq
   auto-save-file-name-transforms
   `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))



(provide 'история)
