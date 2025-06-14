;;; title-mode.el --- Minimal buffer name as window header -*- lexical-binding: t; -*-
;;
;; Show short buffer name at top of each visible window (as a replacement for modeline).

(defgroup title-mode nil
  "Show short buffer name above each window as a header."
  :group 'convenience)

(defface title-mode-header-face
  '((t :inherit mode-line :weight bold :box t
        :background "#222222"))
  "Face for childframe window buffer titles for title-mode.")

;; Map from window to its childframe
(defvar title-mode--window-childframes (make-hash-table :test 'eq)
  "Hashtable from window to child frame showing its buffer title.")

(defun title-mode--eligible-window-p (win)
  (and (window-live-p win)
       (not (window-minibuffer-p win))))

(defun title-mode--short-name (buf)
  (or (and (buffer-file-name buf)
           (file-name-nondirectory (buffer-file-name buf)))
      (buffer-name buf)))

(defun title-mode--make-childframe (win string)
  "Create and return a child frame for WIN displaying STRING."
  (let* ((parent-frame (window-frame win))
         (geometry (window-edges win nil parent-frame 'pixel))
         (x (nth 0 geometry))
         (y (nth 1 geometry))
         (width (window-pixel-width win))
         (params
          `((parent-frame . ,parent-frame)
            (left . ,x)
            (top . ,y)
            (width . ,(max 10 (length string)))
            (height . 1)
            (undecorated . t)
            (minibuffer . nil)
            (border-width . 0)
            (no-accept-focus . t)
            (child-frame-border-width . 0)
            (vertical-scroll-bars . nil)
            (horizontal-scroll-bars . nil)
            (left-fringe . 0)
            (right-fringe . 0)
            (no-other-frame . t)
            (skip-taskbar . t)
            (no-special-glyphs . t)
            (unsplittable . t)
            (desktop-dont-save . t)
            (internal-border-width . 8)
            (background-color . ,(face-background 'title-mode-header-face nil t)))))
    (let ((buf (generate-new-buffer " *title-mode-childframe*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert string)
        (let ((face-remapping-alist
               `((default title-mode-header-face))))
          (setq-local face-remapping-alist face-remapping-alist)))
      (let ((child (display-buffer-in-child-frame buf `((child-frame-parameters . ,params)))))
        ;; Resize to actual buffer content
        (set-frame-size (window-frame (get-buffer-window buf t))
                        (length string) 1 t)
        child))))

(defun title-mode--delete-childframe (win)
  (let ((cf (gethash win title-mode--window-childframes)))
    (when (and cf (frame-live-p (window-frame cf)))
      (let ((cf-frame (window-frame cf)))
        (delete-frame cf-frame t)))
    (remhash win title-mode--window-childframes)))


(defun title-mode--set ()
  "Show short buffer name at the top of every eligible window using child frames."
  ;; Remove stale frames for dead windows
  (maphash (lambda (win _)
             (unless (window-live-p win)
               (title-mode--delete-childframe win)))
           title-mode--window-childframes)
  ;; For each eligible window, create/update a childframe
  (dolist (win (window-list nil 'no-mini))
    (when (title-mode--eligible-window-p win)
      (let* ((buf (window-buffer win))
             (short (title-mode--short-name buf))
             (cf (gethash win title-mode--window-childframes)))
        (if (and cf (window-live-p cf))
            (with-current-buffer (window-buffer cf)
              (erase-buffer)
              (insert short))
          (puthash win (title-mode--make-childframe win short) title-mode--window-childframes))))))

(defun title-mode--unset ()
  "Destroy all childframes used for title display."
  (maphash (lambda (win _)
             (title-mode--delete-childframe win))
           title-mode--window-childframes)
  (clrhash title-mode--window-childframes)

  ;; Clean up possible orphan buffers
  (dolist (buf (buffer-list))
    (when (string-prefix-p " *title-mode-childframe*" (buffer-name buf))
      (kill-buffer buf))))

(defvar title-mode--inhibit-refresh nil
  "Non-nil means do not re-enter title-mode--refresh.")

(defun title-mode--refresh (&rest _)
  (unless title-mode--inhibit-refresh
    (let ((title-mode--inhibit-refresh t))
      (when title-mode
        (title-mode--unset)
        (title-mode--set)))))

;;;###autoload
(define-minor-mode title-mode
  "Show short buffer name at the top of each window using child frames."
  :global t
  (if title-mode
      (progn
        (add-hook 'window-configuration-change-hook #'title-mode--refresh)
        (add-hook 'buffer-list-update-hook #'title-mode--refresh)
        (add-hook 'window-buffer-change-functions #'title-mode--refresh)
        (title-mode--set))
    (remove-hook 'window-configuration-change-hook #'title-mode--refresh)
    (remove-hook 'buffer-list-update-hook #'title-mode--refresh)
    (remove-hook 'window-buffer-change-functions #'title-mode--refresh)
    (title-mode--unset)))

(provide 'title-mode)
