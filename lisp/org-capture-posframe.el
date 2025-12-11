;;; org-capture-posframe.el --- Show Org Capture in a posframe (top-center) without breaking windows -*- lexical-binding: t; -*-

;; Requirements: Emacs 26+ (child frames) and posframe package.
;; Usage:
;;   (require 'org-capture-posframe)
;;   (org-capture-posframe-mode 1)
;;
;; This redirects "*Org Select*" and "*Org Capture*" buffers into posframes,
;; positioned at the top-center of the current frame. Normal window layout is left intact.

(defgroup org-capture-posframe nil
  "Display org-capture buffers in a posframe."
  :group 'org
  :prefix "org-capture-posframe-")

(defcustom org-capture-posframe-poshandler #'posframe-poshandler-frame-top-center
  "Poshandler used to place the posframe."
  :type 'function)

(defcustom org-capture-posframe-width  (lambda () (min 100 (round (* 0.8 (frame-width)))))
  "Width of the posframe (in columns) or a function returning it."
  :type '(choice integer function))

(defcustom org-capture-posframe-height (lambda () (min 22 (round (* 0.6 (frame-height)))))
  "Height of the posframe (in rows) or a function returning it."
  :type '(choice integer function))

(defcustom org-capture-posframe-select-height 12
  "Height (rows) for the template selection buffer \"*Org Select*\"."
  :type 'integer)

(defcustom org-capture-posframe-border-width 8
  "Internal border width for the posframe."
  :type 'integer)

(defcustom org-capture-posframe-parameters
  '((undecorated . t)
    (internal-border-width . 8)
    (minibuffer . nil)
    (drag-internal-border . t)
    (no-accept-focus . nil)
    (no-other-frame . t)
    (skip-taskbar . t)
    (no-special-glyphs . t))
  "Additional frame parameters for the posframe."
  :type '(alist :key-type symbol :value-type sexp))

(defvar org-capture-posframe--rules nil
  "Display buffer rules installed by org-capture-posframe-mode.")

(defun org-capture-posframe--value (v)
  "Return value V; if V is a function, call it without args."
  (if (functionp v) (funcall v) v))

(defun org-capture-posframe--buffer-is (buffer regexp)
  "Return non-nil if BUFFER's name matches REGEXP."
  (let ((name (if (bufferp buffer) (buffer-name buffer) (format "%s" buffer))))
    (and name (string-match-p regexp name))))

(defun org-capture-posframe--show (buffer)
  "Show BUFFER in a posframe and focus it. Return the created frame or nil."
  (require 'posframe)
  (let* ((is-select (org-capture-posframe--buffer-is buffer "\\*Org Select\\*"))
         (width  (org-capture-posframe--value org-capture-posframe-width))
         (height (if is-select
                     org-capture-posframe-select-height
                   (org-capture-posframe--value org-capture-posframe-height))))
    (posframe-show
     buffer
     :poshandler org-capture-posframe-poshandler
     :width width
     :height height
     :internal-border-width org-capture-posframe-border-width
     :accept-focus t
     :respect-header-line t
     :respect-mode-line t
     :override-parameters org-capture-posframe-parameters
     :background-color (face-background 'default nil t)
     :foreground-color (face-foreground 'default nil t))))

(defun org-capture-posframe--display-buffer (buffer _alist)
  "Display BUFFER in a posframe.
Return nil so display-buffer accepts no-window (see allow-no-window)."
  (when (require 'posframe nil t)
    (when (or (org-capture-posframe--buffer-is buffer "\\*Org Capture\\*")
              (org-capture-posframe--buffer-is buffer "\\*Org Select\\*"))
      (when-let ((frm (org-capture-posframe--show buffer)))
        (select-frame-set-input-focus frm))))
  ;; We purposely return nil and rely on (allow-no-window . t) to avoid fallback.
  nil)

(defun org-capture-posframe--cleanup ()
  "Hide/delete posframes used by org-capture."
  (when (featurep 'posframe)
    (dolist (bn '("*Org Capture*" "*Org Select*"))
      (ignore-errors (posframe-hide bn))
      (ignore-errors (posframe-delete bn)))))

(defun org-capture-posframe--on-capture-mode ()
  "When capture buffer appears, remove the select posframe."
  (ignore-errors (org-capture-posframe--cleanup)))

;;;###autoload
(define-minor-mode org-capture-posframe-mode
  "Show Org Capture/Select buffers in posframes to preserve window layout."
  :global t
  :group 'org-capture-posframe
  (if org-capture-posframe-mode
      (progn
        (unless (and (fboundp 'child-frame-p)
                     (require 'posframe nil t))
          (setq org-capture-posframe-mode nil)
          (user-error "org-capture-posframe-mode: posframe not available or Emacs lacks child frame support"))
        ;; Install display rules (highest priority)
        (let ((rule
               '("\\*Org \\(Capture\\|Select\\)\\*"
                 org-capture-posframe--display-buffer
                 (allow-no-window . t))))
          (setq org-capture-posframe--rules (list rule))
          (dolist (r (reverse org-capture-posframe--rules))
            (add-to-list 'display-buffer-alist r)))
        ;; Hooks to clean up frames
        (add-hook 'org-capture-mode-hook #'org-capture-posframe--on-capture-mode)
        (add-hook 'org-capture-after-finalize-hook #'org-capture-posframe--cleanup))
    ;; Disable: remove rules and hooks, cleanup frames
    (dolist (r org-capture-posframe--rules)
      (setq display-buffer-alist (delete r display-buffer-alist)))
    (setq org-capture-posframe--rules nil)
    (remove-hook 'org-capture-mode-hook #'org-capture-posframe--on-capture-mode)
    (remove-hook 'org-capture-after-finalize-hook #'org-capture-posframe--cleanup)
    (org-capture-posframe--cleanup)))

(provide 'org-capture-posframe)
;;; org-capture-posframe.el ends here
