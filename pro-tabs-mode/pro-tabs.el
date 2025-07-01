;;; pro-tabs.el --- Modern tabs, tab-bar and tab-line enhancements for Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Peter Kosov

;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/pro-tabs
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (all-the-icons "5.0.0") (use-package "2.4"))
;; Keywords: convenience, tabs, ui

;;; Commentary:

;; pro-tabs is a package for power users who want clever, customizable, and aesthetic tabs
;; in Emacs. It enhances Emacs' built-in tab-bar and tab-line, providing icon support,
;; compact design, better visuals, buffer/tab management utilities, and more.

;; Features:
;; - Beautiful tab-bar and tab-line formatting with icons (requires all-the-icons)
;; - No ugly tab new/close buttons by default
;; - Custom tab separator shapes (wave pixmaps)
;; - Current tab highlighting
;; - One-key switching and tab closing
;; - Auto-activation in vterm/telega

;; Quick Start:
;;   1. Install all-the-icons: M-x package-install RET all-the-icons RET
;;   2. Place pro-tabs.el in your load-path
;;   3. (require 'pro-tabs) in your init
;;
;; Tab-bar will activate globally. Use `pro-tabs-open-new-tab` to open a new tab with dashboard,
;; and `pro-tabs-close-tab-and-buffer` to close the active buffer and its tab.
;;
;; You can customize tab-bar and tab-line appearance as you wish.

;;; Code:

;; --- Customization group and variables ---

(defgroup pro-tabs nil
  "Enhanced tab-bar and tab-line experience."
  :group 'convenience
  :prefix "pro-tabs-")

(defcustom pro-tabs-enable-icons t
  "Whether to show icons in tab-bar and tab-line."
  :type 'boolean
  :group 'pro-tabs)

(defcustom pro-tabs-max-tab-name-length 25
  "Maximum length of tab name before it is truncated with ellipsis."
  :type 'integer
  :group 'pro-tabs)

(defcustom pro-tabs-tab-bar-height 18
  "Height (in pixels) for pro-tabs tab-bar formatting."
  :type 'integer
  :group 'pro-tabs)

(defcustom pro-tabs-tab-line-height 20
  "Height (in pixels) for pro-tabs tab-line formatting."
  :type 'integer
  :group 'pro-tabs)

;; --- Pure formatting and wave XPM utilities (no side effects) ---
;;
;; Functions below are pure: they never mutate global state or buffers.
;; Used for generating visually appealing tab separators in tab-bar/tab-line.

(defun pro-tabs--safe-face-background (face)
  "Return the background color of FACE or \"None\" if unavailable.
FACE may be a symbol or nil. Never errors."
  (let ((color (and (symbolp face) (facep face) (face-background face nil t))))
    (if (and color (not (equal color ""))) color "None")))

(defun pro-tabs--safe-interpolated-color (face1 face2)
  "Return the blended color between FACE1 and FACE2, as #RRGGBB or \"None\".
Used for smooth gradient effect in XPM wave separators."
  (let* ((c1 (pro-tabs--safe-face-background face1))
         (c2 (pro-tabs--safe-face-background face2)))
    (condition-case nil
        (if (and (not (equal c1 "None"))
                 (not (equal c2 "None")))
            (apply 'color-rgb-to-hex
                   (cl-mapcar (lambda (a b) (/ (+ a b) 2))
                              (color-name-to-rgb c1)
                              (color-name-to-rgb c2)))
          "None")
      (error "None"))))

(defun pro-tabs--wave-left (face1 face2 &optional height)
  "Generate left wave XPM separator (pure function).
Argument FACE1 is the 'current' face; FACE2 is the neighbor face; HEIGHT is height in px.
Returns a display property suitable for :display."
  (let* ((height (or height (frame-char-height)))
         (template [  "21111111111"
                      "00111111111"
                      "00011111111"
                      "00021111111"
                      "00001111111"
                      "00002111111"
                      "00000111111"
                      "00000111111"
                      "00000211111"
                      "00000021111"
                      "00000001111"
                      "00000001111"
                      "00000002111"
                      "00000000111"
                      "00000000211"
                      "00000000002"])
         (lines nil))
    (dotimes (i height)
      (push (aref template (floor (* i (/ (float (length template)) height)))) lines))
    (let ((img (create-image
                (concat
                 "/* XPM */\nstatic char * wave_left_xpm[] = {\n"
                 (format "\"11 %d 3 1\", " height)
                 "\"0 c " (pro-tabs--safe-face-background face2)
                 "\", \"1 c " (pro-tabs--safe-face-background face1)
                 "\", \"2 c " (pro-tabs--safe-interpolated-color face2 face1)
                 "\",\n"
                 (mapconcat (lambda (l) (format "\"%s\"," l)) (nreverse lines) "\n")
                 "\"};\n")
                'xpm t :ascent 'center)))
      (list 'image :type 'xpm :data (plist-get (cdr img) :data) :ascent 'center :face face2))))

(defun pro-tabs--wave-right (face1 face2 &optional height)
  "Generate right wave XPM separator (mirror of left) as pure function.
Argument FACE1 is left face; FACE2 is right face; HEIGHT is height in px.
Returns a display property suitable for :display."
  (let* ((height (or height (frame-char-height)))
         (left-template [
                      "21111111111"
                      "00111111111"
                      "00011111111"
                      "00021111111"
                      "00001111111"
                      "00002111111"
                      "00000111111"
                      "00000111111"
                      "00000211111"
                      "00000021111"
                      "00000001111"
                      "00000001111"
                      "00000002111"
                      "00000000111"
                      "00000000211"
                      "00000000002"])
         (lines nil))
    (dotimes (i height)
      (let* ((orig (aref left-template (floor (* i (/ (float (length left-template)) height)))))
             (mirrored (apply #'string (nreverse (string-to-list orig)))))
        (push mirrored lines)))
    (let ((img (create-image
                (concat
                 "/* XPM */\nstatic char * wave_right_xpm[] = {\n"
                 (format "\"11 %d 3 1\", " height)
                 "\"0 c " (pro-tabs--safe-face-background face1)
                 "\", \"1 c " (pro-tabs--safe-face-background face2)
                 "\", \"2 c " (pro-tabs--safe-interpolated-color face1 face2)
                 "\",\n"
                 (mapconcat (lambda (l) (format "\"%s\"," l)) (nreverse lines) "\n")
                 "\"};\n")
                'xpm t :ascent 'center)))
      (list 'image :type 'xpm :data (plist-get (cdr img) :data) :ascent 'center :face face1))))

(defun pro-tabs--replace-strings (replacements string)
  "Replace all keys from REPLACEMENTS alist in STRING with corresponding values.
Pure utility used for beautified tab-names."
  (seq-reduce
   (lambda (str pair)
     (string-replace
      (car pair)
      (cdr pair)
      str))
   replacements
   string))

(require 'all-the-icons) ;; for icon support

(defvar pro-tabs--old-tab-bar-new-button-show nil)
(defvar pro-tabs--old-tab-bar-close-button-show nil)
(defvar pro-tabs--old-tab-bar-separator nil)
(defvar pro-tabs--old-tab-bar-auto-width nil)
(defvar pro-tabs--old-s-keys nil)

;; --- Tab-bar/tab-line formatting functions (pure, public API) ---
;;
;; These are pure: they use only args or defcustoms, never setq etc.
;; They are designed for assignment to tab-bar-tab-name-format-function and tab-line-tab-name-function.

(defun pro-tabs-format-tab-bar (tab i)
  "Return formatted string for TAB (alist from Emacs). Used as `tab-bar-tab-name-format-function'.
Second arg I is the tab index (unused). Never mutates global state."
  (let* ((tab-height pro-tabs-tab-bar-height)
         (tab-name-length pro-tabs-max-tab-name-length)
         (icon-default (when pro-tabs-enable-icons
                         (all-the-icons-octicon "browser" :height 1 :v-adjust 0.1)))
         (icon-firefox (when pro-tabs-enable-icons
                         (all-the-icons-faicon "firefox" :height 1 :v-adjust 0)))
         (icon-chrome (when pro-tabs-enable-icons
                        (all-the-icons-faicon "chrome" :height 1 :v-adjust 0)))
         (tab-name-replacements `(("Firefox-esr" . ,icon-firefox)
                                  ("firefox-default" . ,icon-firefox)
                                  ("Google-chrome" . ,icon-chrome)))
         (current-tab? (eq (car tab) 'current-tab))
         (buffer-name (substring-no-properties (alist-get 'name tab)))
         (tab-major-mode (if (bufferp (get-buffer buffer-name))
                             (with-current-buffer buffer-name major-mode) nil))
         (icon-mode (when pro-tabs-enable-icons
                      (all-the-icons-icon-for-mode tab-major-mode :height 0.8)))
         (icon-tab (cond ((not pro-tabs-enable-icons) "")
                         ((symbolp icon-mode) icon-default)
                         (t icon-mode)))
         (tab-face (if current-tab? 'tab-bar-tab 'tab-bar-tab-inactive))
         (wave-right (propertize " " 'display
                                 (pro-tabs--wave-right
                                  tab-face
                                  'tab-bar
                                  (+ 1 tab-height))))
         (wave-left (propertize " " 'display
                                (pro-tabs--wave-left
                                 'tab-bar
                                 tab-face
                                 (+ 1 tab-height))))
         (shortened-name (pro-tabs--replace-strings tab-name-replacements buffer-name))
         (final-tab-name (format "%s" (if (> (length shortened-name) tab-name-length)
                                          (concat
                                           (substring shortened-name 0 tab-name-length) "…")
                                        shortened-name)))
         (tab-text (concat
                    wave-right
                    " "
                    icon-tab
                    " "
                    final-tab-name
                    " "
                    wave-left)))
    (add-face-text-property 0 (length tab-text) tab-face t tab-text)
    tab-text))

;; Set these only from pro-tabs-mode, not on load!

;;;###autoload
(defun pro-tabs-open-new-tab ()
  "Open a new tab with dashboard, if present."
  (interactive)
  (tab-bar-new-tab-to)
  (when (fboundp 'dashboard-open)
    (dashboard-open)))

;; --- tab-line ---
(defvar pro-tabs--old-tab-line-new-button-show nil)
(defvar pro-tabs--old-tab-line-close-button-show nil)
(defvar pro-tabs--old-tab-line-separator nil)
(defvar pro-tabs--old-tab-line-switch-cycling nil)
(defvar pro-tabs--old-tab-line-tabs-function nil)
(defvar pro-tabs--tab-line-global-keys nil)

(defun pro-tabs-format-tab-line (buffer &optional _buffers)
  "Return formatted display for BUFFER for use with `tab-line-tab-name-function`.
Shows a wave separator left/right. Pure function."
  (let* ((is-current (eq buffer (window-buffer)))
         (_face-tab (if is-current 'tab-line-tab-current 'tab-line-tab-inactive))
         (bname (format "%s" (buffer-name buffer))))
    (concat
     (propertize " " 'display (pro-tabs--wave-right nil 'tab-line (+ 1 pro-tabs-tab-line-height)))
     bname
     (propertize " " 'display (pro-tabs--wave-left 'tab-line nil (+ 1 pro-tabs-tab-line-height))))))

;; Set this only from pro-tabs-mode, not on load!

;;;###autoload
(defun pro-tabs-close-tab-and-buffer ()
  "Close the current tab and its buffer."
  (interactive)
  (kill-this-buffer)
  (tab-close))

;;;###autoload
(define-minor-mode pro-tabs-mode
  "Toggle pro-tabs enhancements globally."
  :global t
  :group 'pro-tabs
  (if pro-tabs-mode
      (progn
        ;; save old values
        (setq pro-tabs--old-tab-bar-new-button-show tab-bar-new-button-show)
        (setq pro-tabs--old-tab-bar-close-button-show tab-bar-close-button-show)
        (setq pro-tabs--old-tab-bar-separator tab-bar-separator)
        (setq pro-tabs--old-tab-bar-auto-width tab-bar-auto-width)
        (when (boundp 'tab-line-new-button-show)
          (setq pro-tabs--old-tab-line-new-button-show tab-line-new-button-show))
        (when (boundp 'tab-line-close-button-show)
          (setq pro-tabs--old-tab-line-close-button-show tab-line-close-button-show))
        (when (boundp 'tab-line-separator)
          (setq pro-tabs--old-tab-line-separator tab-line-separator))
        (when (boundp 'tab-line-switch-cycling)
          (setq pro-tabs--old-tab-line-switch-cycling tab-line-switch-cycling))
        (when (boundp 'tab-line-tabs-function)
          (setq pro-tabs--old-tab-line-tabs-function tab-line-tabs-function))

        ;; Apply our settings
        (setq tab-bar-new-button-show nil)
        (setq tab-bar-close-button-show nil)
        (setq tab-bar-separator " ")
        (setq tab-bar-auto-width nil)
        (tab-bar-mode 1)
        (tab-bar-history-mode 1)
        (setq tab-bar-tab-name-format-function #'pro-tabs-format-tab-bar)
        (setq tab-bar-tab-name-function #'tab-bar-tab-name-current)

        (when (boundp 'tab-line-new-button-show)
          (setq tab-line-new-button-show nil))
        (when (boundp 'tab-line-close-button-show)
          (setq tab-line-close-button-show nil))
        (when (boundp 'tab-line-separator)
          (setq tab-line-separator ""))
        (when (boundp 'tab-line-switch-cycling)
          (setq tab-line-switch-cycling t))
        (when (boundp 'tab-line-tabs-function)
          (setq tab-line-tabs-function 'tab-line-tabs-mode-buffers))
        (when (boundp 'tab-line-tab-name-function)
          (setq tab-line-tab-name-function #'pro-tabs-format-tab-line))
        (ignore-errors (set-face-attribute 'tab-line-tab nil :box nil))
        (ignore-errors (set-face-attribute 'tab-line-tab-current nil :box nil))
        (ignore-errors (set-face-attribute 'tab-line-tab-inactive nil :box nil))

        ;; Keybindings s-0...s-9
        (setq pro-tabs--old-s-keys (make-hash-table))
        (dotimes (i 10)
          (let ((key (kbd (format "s-%d" i))))
            (puthash key (lookup-key global-map key) pro-tabs--old-s-keys)
            (global-set-key key `(lambda () (interactive) (tab-bar-select-tab ,i)))))
        ;; tab-line-mode for vterm/telega
        (add-hook 'vterm-mode-hook #'tab-line-mode)
        (add-hook 'telega-mode-hook #'tab-line-mode)
        ;; Remove C-<tab> global keys
        (with-eval-after-load 'tab-bar
          (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
          (define-key global-map (kbd "C-<tab>") nil)
          (define-key tab-bar-mode-map (kbd "C-<tab>") nil)
          (define-key tab-bar-mode-map (kbd "C-<iso-lefttab>") nil))
        (with-eval-after-load 'tab-line
          (define-key global-map (kbd "C-S-<iso-lefttab>") nil)
          (define-key global-map (kbd "C-<tab>") nil)))
    ;; restore old values and unbind
    (tab-bar-mode 0)
    (tab-bar-history-mode 0)
    (setq tab-bar-new-button-show pro-tabs--old-tab-bar-new-button-show)
    (setq tab-bar-close-button-show pro-tabs--old-tab-bar-close-button-show)
    (setq tab-bar-separator pro-tabs--old-tab-bar-separator)
    (setq tab-bar-auto-width pro-tabs--old-tab-bar-auto-width)
    (setq tab-bar-tab-name-format-function nil)
    (setq tab-bar-tab-name-function nil)
    (when (boundp 'tab-line-new-button-show)
      (setq tab-line-new-button-show pro-tabs--old-tab-line-new-button-show))
    (when (boundp 'tab-line-close-button-show)
      (setq tab-line-close-button-show pro-tabs--old-tab-line-close-button-show))
    (when (boundp 'tab-line-separator)
      (setq tab-line-separator pro-tabs--old-tab-line-separator))
    (when (boundp 'tab-line-switch-cycling)
      (setq tab-line-switch-cycling pro-tabs--old-tab-line-switch-cycling))
    (when (boundp 'tab-line-tabs-function)
      (setq tab-line-tabs-function pro-tabs--old-tab-line-tabs-function))
    (when (boundp 'tab-line-tab-name-function)
      (setq tab-line-tab-name-function nil))
    (ignore-errors (set-face-attribute 'tab-line-tab nil :box t))
    (ignore-errors (set-face-attribute 'tab-line-tab-current nil :box t))
    (ignore-errors (set-face-attribute 'tab-line-tab-inactive nil :box t))
    (remove-hook 'vterm-mode-hook #'tab-line-mode)
    (remove-hook 'telega-mode-hook #'tab-line-mode)
    ;; Restore s-0..9
    (when (hash-table-p pro-tabs--old-s-keys)
      (maphash (lambda (key fun)
                 (global-set-key key fun))
               pro-tabs--old-s-keys))))

(provide 'pro-tabs)
;;; pro-tabs.el ends here
