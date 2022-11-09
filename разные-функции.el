(defun подключить-модуль (feature &optional filename) 
  "Как `require', но с аккуратной обработкой ошибок.

Если возникнет ошибка, её текст будет добавлен в сообщение.

Как `require', вернёт FEATURE если всё ок, и nil если нет."
  (condition-case err 
      (require feature filename) 
    (error 
     (message "Ошибка загрузки %s: \"%s\"" (if filename (format "%s (%s)" feature filename) feature) 
              (error-message-string err))
     nil)))

;; (defun center-windows () 
;;   "visually center all text in buffers by add proportional margins" 
;;   (interactive) 
;;   (mapc 
;;    (lambda (win) 
;;      (let* 
;;          ((used-width 
;;            100) 
;;           (wh (window-total-width win)) 
;;           (mg (/ (- wh used-width) 2) )) 
;;        (if (< mg 1) 
;;            (setq mg 0)) ;; Do not add right margin, for long lines visibility
;;        (set-window-margins win mg 0) 
;;        (window-width))) 
;;    (delete (minibuffer-window) 
;;            (window-list))))

;; (defun increase-margin () 
;;   "increase left margin" 
;;   (interactive) 
;;   (let 
;;       ((mn 
;;         (or (car (window-margins)) 
;;             0))) 
;;     (scroll-right nil t) 
;;     (set-window-margins nil (+ 5 mn))))

;; (defun decrease-margin () 
;;   "decrease left margin" 
;;   (interactive) 
;;   (let 
;;       ((mn 
;;         (or (car (window-margins)) 
;;             0)))
;;     (if (> mn 0) 
;;         (set-window-margins nil (- mn 5)) 
;;       (scroll-left 5))))

;; (defun toggle-mode-line () 
;;   "toggles the modeline on and off" 
;;   (interactive) 
;;   (setq mode-line-format (if (equal mode-line-format nil) 
;;                              (default-value 'mode-line-format)) ) 
;;   (redraw-display))

;; (defun toggle-header-line () 
;;   "toggles the modeline on and off" 
;;   (interactive) 
;;   (setq header-line-format (if (equal header-line-format nil) 
;;                                (default-value 'header-line-format)) ) 
;;   (redraw-display))

;; (defun edit-current-file-as-root () 
;;   "Edit as root the file associated with the current buffer" 
;;   (interactive) 
;;   (if (buffer-file-name) 
;;       (progn 
;;         (setq file (concat "/sudo:root@localhost:" (buffer-file-name))) 
;;         (find-file file)) 
;;     (message "Buffer is not associated to a file.")))

;; (defun my/minibuffer-keyboard-quit () 
;;   "Abort recursive edit.
;;   In Delete Selection mode, if the mark is active, just deactivate it;
;;   then it takes a second \\[keyboard-quit] to abort the minibuffer." 
;;   (interactive) 
;;   (if (and delete-selection-mode 
;;            transient-mark-mode
;;            mark-active) 
;;       (setq deactivate-mark t) 
;;     (when (get-buffer "*Completions*") 
;;       (delete-windows-on "*Completions*")) 
;;     (abort-recursive-edit)))

;; (defun my/find-file-recent () 
;;   "Find a recent file using ido." 
;;   (interactive) 
;;   (let 
;;       ((file 
;;         (ido-completing-read "Choose recent file: " recentf-list nil t))) 
;;     (when file (find-file file))))

;; (defun my/evil-shift-left () 
;;   "Evil mode shift region to left" 
;;   (interactive) 
;;   (evil-shift-left (region-beginning) 
;;                    (region-end)) 
;;   (evil-normal-state) 
;;   (evil-visual-restore))

;; (defun my/evil-shift-right () 
;;   "Evil mode shift region to right" 
;;   (interactive) 
;;   (evil-shift-right (region-beginning) 
;;                     (region-end)) 
;;   (evil-normal-state) 
;;   (evil-visual-restore))

;; (defun switch-recent-buffers () 
;;   "Switch to previously open buffer.
;; Repeated invocations toggle between the two most recently open buffers." 
;;   (interactive) 
;;   (switch-to-buffer (other-buffer (current-buffer) 1)))


;; (defun paste-from-x () 
;;   "Paste text from X" 
;;   (interactive) 
;;   (call-process "xsel" nil t))


;; (defun vi-open-line-above () 
;;   "Insert a newline above the current line and put point at beginning." 
;;   (interactive) 
;;   (unless (bolp) 
;;     (beginning-of-line)) 
;;   (newline) 
;;   (forward-line -1) 
;;   (indent-according-to-mode))

;; (defun vi-open-line-below () 
;;   "Insert a newline below the current line and put point at beginning." 
;;   (interactive) 
;;   (unless (eolp) 
;;     (end-of-line)) 
;;   (newline-and-indent))

;; (defun vi-open-line 
;;     (&optional 
;;      abovep)
;;   "Insert a newline below the current line and put point at beginning.
;; With a prefix argument, insert a newline above the current line." 
;;   (interactive "P") 
;;   (if abovep (vi-open-line-above) 
;;     (vi-open-line-below)))

;; (defun kill-current-line 
;;     (&optional 
;;      n) 
;;   (interactive "p") 
;;   (save-excursion (beginning-of-line) 
;;                   (let 
;;                       ((kill-whole-line 
;;                         t)) 
;;                     (kill-line n))))

;; (defun mark-current-line ()
;;   "Select the current line" 
;;   (interactive) 
;;   (end-of-line)                         ; move to end of line
;;   (set-mark (line-beginning-position)))

;; (defun back-to-indentation-or-beginning-of-line () 
;;   "Moves point back to indentation if there is any
;; non blank characters to the left of the cursor.
;; Otherwise point moves to beginning of line." 
;;   (interactive) 
;;   (if (= (point) 
;;          (save-excursion (back-to-indentation) 
;;                          (point))) 
;;       (beginning-of-line) 
;;     (back-to-indentation)))

;; (defun duplicate-current-line-or-region (arg) 
;;   "Duplicates the current line or region ARG times.
;; If there's no region, the current line will be duplicated. However, if
;; there's a region, all lines that region covers will be duplicated." 
;;   (interactive "p") 
;;   (let (beg end (origin (point))) 
;;     (if (and (region-active-p) 
;;              (> (point) 
;;                 (mark))) 
;;         (exchange-point-and-mark)) 
;;     (setq beg (line-beginning-position)) 
;;     (if (region-active-p) 
;;         (exchange-point-and-mark)) 
;;     (setq end (line-end-position)) 
;;     (let 
;;         ((region 
;;           (buffer-substring-no-properties 
;;            beg
;;            end))) 
;;       (dotimes (i arg) 
;;         (goto-char end) 
;;         (newline) 
;;         (insert region) 
;;         (setq end (point))) 
;;       (goto-char (+ origin (* (length region) arg) arg)))))

;; (defun join-line-or-lines-in-region () 
;;   "Join this line or the lines in the selected region." 
;;   (interactive) 
;;   (cond 
;;    ((region-active-p) 
;;     (let 
;;         ((min 
;;           (line-number-at-pos (region-beginning)))) 
;;       (goto-char (region-end)) 
;;       (while (> (line-number-at-pos) min) 
;;         (join-line)))) 
;;    (t (call-interactively 'join-line))))

;; (defun sp-kill-sexp-with-a-twist-of-lime () 
;;   (interactive) 
;;   (if (sp-point-in-string) 
;;       (let 
;;           ((end 
;;             (plist-get (sp-get-string) 
;;                        :end))) 
;;         (kill-region (point) 
;;                      (1- end))) 
;;     (let ((beg (line-beginning-position)) 
;;           (end (line-end-position))) 
;;       (if (or (comment-only-p beg end) 
;;               (s-matches? "\\s+" 
;;                           (buffer-substring-no-properties 
;;                            beg
;;                            end))) 
;;           (kill-line) 
;;         (sp-kill-sexp)))))

;; (defun move-text-internal (arg) 
;;   (cond 
;;    ((and 
;;      mark-active
;;      transient-mark-mode) 
;;     (if (> (point) 
;;            (mark)) 
;;         (exchange-point-and-mark)) 
;;     (let 
;;         ((column 
;;           (current-column)) 
;;          (text (delete-and-extract-region (point) 
;;                                           (mark)))) 
;;       (forward-line arg) 
;;       (move-to-column column t) 
;;       (set-mark (point)) 
;;       (insert text) 
;;       (exchange-point-and-mark) 
;;       (setq deactivate-mark nil))) 
;;    (t (let 
;;           ((column 
;;             (current-column))) 
;;         (beginning-of-line) 
;;         (when (or (> arg 0) 
;;                   (not (bobp))) 
;;           (forward-line) 
;;           (when (or (< arg 0) 
;;                     (not (eobp))) 
;;             (transpose-lines arg) 
;;             (when (and (eval-when-compile '(and (>= emacs-major-version 24) 
;;                                                 (>= emacs-minor-version 3))) 
;;                        (< arg 0)) 
;;               (forward-line -1))) 
;;           (forward-line -1)) 
;;         (move-to-column column t)))))

;; (defun move-text-down (arg) 
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines down." 
;;   (interactive "*p") 
;;   (move-text-internal arg))

;; (defun move-text-up (arg) 
;;   "Move region (transient-mark-mode active) or current line
;;   arg lines up." 
;;   (interactive "*p") 
;;   (move-text-internal (- arg)))

;; (defun delete-process-at-point () 
;;   (interactive) 
;;   (let 
;;       ((process 
;;         (get-text-property (point) 'tabulated-list-id))) 
;;     (cond 
;;      ((and 
;;        process
;;        (processp process)) 
;;       (delete-process process) 
;;       (revert-buffer)) 
;;      (t 
;;       (error 
;;        "no process at point!")))))


;; (defun switch-to-window () 
;;   (interactive) 
;;   (window-numbering-mode t) 
;;   (let 
;;       ((num 
;;         (- (read-char "Window>") 48)))
;;     (if (and (> num 0) 
;;              (< num 10)) 
;;         (select-window-by-number num)) 
;;     (window-numbering-mode -1)))

;; (defun comment-or-uncomment-region-or-line () 
;;   "Comments or uncomments the region or the current line if there's no active region." 
;;   (interactive) 
;;   (let (beg end) 
;;     (if (region-active-p) 
;;         (setq beg (region-beginning) end (region-end)) 
;;       (setq beg (line-beginning-position) end (line-end-position))) 
;;     (comment-or-uncomment-region beg end)))

;; (defun rename-file-and-buffer (new-name) 
;;   "Renames both current buffer and file it's visiting to NEW-NAME." 
;;   (interactive "sNew name: ") 
;;   (let 
;;       ((name 
;;         (buffer-name)) 
;;        (filename (buffer-file-name))) 
;;     (if (not filename) 
;;         (message "Buffer '%s' is not visiting a file!" name) 
;;       (if (get-buffer new-name) 
;;           (message "A buffer named '%s' already exists!" new-name) 
;;         (progn (rename-file name new-name 1) 
;;                (rename-buffer new-name) 
;;                (set-visited-file-name new-name) 
;;                (set-buffer-modified-p nil))))))

;; (defun kill-other-buffers () 
;;   "Kill all other buffers." 
;;   (interactive) 
;;   (mapc 'kill-buffer (delq (current-buffer) 
;;                            (remove-if-not 'buffer-file-name (buffer-list)))))


;; (defun move-region (start end n) 
;;   "Move the current region up or down by N lines." 
;;   (interactive "r\np") 
;;   (let 
;;       ((line-text 
;;         (delete-and-extract-region start end))) 
;;     (forward-line n) 
;;     (let ((start (point))) 
;;       (insert line-text) 
;;       (setq deactivate-mark nil) 
;;       (set-mark start))))

;; (defun move-region-up (start end n) 
;;   "Move the current line up by N lines." 
;;   (interactive "r\np") 
;;   (move-region start end (if (null n) -1 (- n))))

;; (defun move-region-down (start end n) 
;;   "Move the current line down by N lines." 
;;   (interactive "r\np") 
;;   (move-region start end (if (null n) 1 n)))


;; (defun my/skewer-snippet () 
;;   "Put the snippet to make a webpage skewer-aware in the clipboard." 
;;   (interactive) 
;;   (kill-new "(function () {
;;     var s = document.createElement(\"script\");
;;     s.src = \"//localhost:8080/skewer\";
;;     document.getElementsByTagName(\"head\")[0].appendChild(s); })()"))

;; (defun define-keys (map defns) 
;;   (interactive "P") 
;;   (if defns (progn (define-key map (car defns) 
;;                      (cadr defns)) 
;;                    (define-keys map (cddr defns)))))

;; (defun delete-to-begin () 
;;   (interactive) 
;;   (kill-line 0))

;; (defun my-dired-create-file (file) 
;;   (interactive (list (read-file-name "Create file: " (concat (dired-current-directory) "")))) 
;;   (write-region "" nil (expand-file-name file) t) 
;;   (dired-add-file file) 
;;   (revert-buffer) 
;;   (dired-goto-file (expand-file-name file)))

;; (defun dired-get-size () 
;;   (interactive) 
;;   (let 
;;       ((files 
;;         (dired-get-marked-files))) 
;;     (with-temp-buffer (apply 'call-process "/usr/bin/du" nil t nil "-sch" files) 
;;                       (message "Size of all marked files: %s" (progn (re-search-backward
;;                                                                       "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$") 
;;                                                                      (match-string 1))))))

;; (defun global-set-keys-iter (lis) 
;;   (when lis 
;;     `((global-set-key 
;;        ,(car lis) 
;;        ,(cadr lis)) 
;;       ,@(global-set-keys-iter (cddr lis)))))

;; (defmacro global-set-keys 
;;     (&rest 
;;      key-action-pairs) 
;;   `(progn ,@(global-set-keys-iter key-action-pairs)))

;; (defmacro global-unset-keys 
;;     (&rest 
;;      keys) 
;;   `(progn ,@(mapcar 
;;              (lambda (x) 
;;                `(global-unset-key ,x))
;;              keys)))

(provide 'разные-функции)
