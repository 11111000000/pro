;; package --- Фикс corfu и мультимонитрной конфигурации -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Advise packages that use posframe for a multi-head setup

(defun get-focused-monitor-geometry ()
  "Get the geometry of the monitor displaying the selected frame in EXWM."
  (let* ((monitor-attrs (frame-monitor-attributes))
        (workarea (assoc 'workarea monitor-attrs))
        (geometry (cdr workarea)))
    (list (nth 0 geometry) ; X
          (nth 1 geometry) ; Y
          (nth 2 geometry) ; Width
          (nth 3 geometry) ; Height
          )))

(defun advise-corfu-make-frame-with-monitor-awareness (orig-fun frame x y width height)
  "Advise `corfu--make-frame` to be monitor-aware, adjusting X and Y according to the focused monitor."
  ;; Get the geometry of the currently focused monitor
  (let* ((monitor-geometry (get-focused-monitor-geometry))
        (monitor-x (nth 0 monitor-geometry))
        (monitor-y (nth 1 monitor-geometry))
        ;; You may want to adjust the logic below if you have specific preferences
        ;; on where on the monitor the posframe should appear.
        ;; Currently, it places the posframe at its intended X and Y, but ensures
        ;; it's within the bounds of the focused monitor.
        (new-x (+ monitor-x x))
        (new-y (+ monitor-y y)))

    ;; Call the original function with potentially adjusted coordinates
    (funcall orig-fun frame new-x new-y width height)))


(advice-add 'corfu--make-frame :around #'advise-corfu-make-frame-with-monitor-awareness)


;; (defun corfu--make-frame (frame x y width height buffer)
;;   "Show BUFFER in child frame at X/Y with WIDTH/HEIGHT.
;; FRAME is the existing frame."
;;   (when-let (((frame-live-p frame))
;;            (timer (frame-parameter frame 'corfu--hide-timer)))
;;     (cancel-timer timer)
;;     (set-frame-parameter frame 'corfu--hide-timer nil))
;;   (let* ((window-min-height 1)
;;         (window-min-width 1)
;;         (inhibit-redisplay t)
;;         (x-gtk-resize-child-frames corfu--gtk-resize-child-frames)
;;         (before-make-frame-hook)
;;         (after-make-frame-functions)
;;         (parent (window-frame)))
;;     (unless (and (frame-live-p frame)
;;              (eq (frame-parent frame)
;;                   (and (not (bound-and-true-p exwm--connection)) parent))
;;              ;; If there is more than one window, `frame-root-window' may
;;              ;; return nil.  Recreate the frame in this case.
;;              (window-live-p (frame-root-window frame)))
;;       (when frame (delete-frame frame))
;;       (setq frame (make-frame
;;                   `((parent-frame . ,parent)
;;                     (minibuffer . ,(minibuffer-window parent))
;;                     (width . 0) (height . 0) (visibility . nil)
;;                     ,@corfu--frame-parameters))))
;;     ;; XXX HACK Setting the same frame-parameter/face-background is not a nop.
;;     ;; Check before applying the setting. Without the check, the frame flickers
;;     ;; on Mac. We have to apply the face background before adjusting the frame
;;     ;; parameter, otherwise the border is not updated.
;;     (let ((new (face-attribute 'corfu-border :background nil 'default)))
;;       (unless (equal (face-attribute 'internal-border :background frame 'default) new)
;;         (set-face-background 'internal-border new frame)))
;;     ;; Reset frame parameters if they changed.  For example `tool-bar-mode'
;;     ;; overrides the parameter `tool-bar-lines' for every frame, including child
;;     ;; frames.  The child frame API is a pleasure to work with.  It is full of
;;     ;; lovely surprises.
;;     (when-let ((params (frame-parameters frame))
;;              (reset (seq-remove
;;                      (lambda (p) (equal (alist-get (car p) params) (cdr p)))
;;                      `((background-color
;;                         . ,(face-attribute 'corfu-default :background nil 'default))
;;                        (font . ,(frame-parameter parent 'font))
;;                        ,@corfu--frame-parameters))))
;;       (modify-frame-parameters frame reset))
;;     (let ((win (frame-root-window frame)))
;;       (unless (eq (window-buffer win) buffer)
;;         (set-window-buffer win buffer))
;;       ;; Disallow selection of root window (gh:minad/corfu#63)
;;       (set-window-parameter win 'no-delete-other-windows t)
;;       (set-window-parameter win 'no-other-window t)
;;       ;; Mark window as dedicated to prevent frame reuse (gh:minad/corfu#60)
;;       (set-window-dedicated-p win t))
;;     (redirect-frame-focus frame parent)
;;     (set-frame-size frame width height t)
;;     (unless (equal (frame-position frame) (cons x y))
;;       (if (bound-and-true-p exwm--connection)          
;;           (set-frame-position
;;            frame
;;            (+ x (car (frame-monitor-geometry exwm-workspace--current)))
;;            (+ y (car (cdr (frame-monitor-geometry exwm-workspace--current)))))
;;         (set-frame-position frame x y))
;;       ))
;;   (make-frame-visible frame)
;;   ;; Unparent child frame if EXWM is used, otherwise EXWM buffers are drawn on
;;   ;; top of the Corfu child frame.
;;   (when (and (bound-and-true-p exwm--connection) (frame-parent frame))
;;     (set-frame-parameter frame 'parent-frame nil))
;;   frame)


(provide 'про-фикс-corfu)
;;; про-фикс-corfu.el ends here

