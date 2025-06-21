;;; gptel-context-store.el --- Save and load gptel context from files  -*- lexical-binding: t; -*-

;; Author: Peter Kosov <11111000000@email.com>
;; URL: https://github.com/11111000000/gptel-context-store
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (transient "0.3"))
;; Keywords: convenience, gptel, context

;;; Commentary:

;; This package provides interactive commands to save and load the gptel context 
;; (`gptel-context--alist`) from a file, with a default filename ".gptel-context.el" 
;; in the current directory. Also adds these actions to the gptel transient menu.
;;
;; Features:
;; - Context is serialized to a portable alist of filenames and regions.
;; - Prompt for filename with default.
;; - Load supports merging or replacing the current context.
;; - Extend the gptel transient menu with keys 'S' (Save context) and 'L' (Load context).
;;
;; Usage:
;; 1) Place this file in your load-path.
;; 2) Add `(require 'gptel-context-store)` to your Emacs init file after gptel.
;; 3) The commands `gptel-context-store-save` and `gptel-context-store-load`
;;    become available.
;; 4) In gptel's transient menu, 'S' saves context, 'L' loads context.
;;
;; To publish on GitHub:
;; - Create a repository named "gptel-context-store".
;; - Add this file as `gptel-context-store.el`.
;; - Add a README.md describing usage and installation.
;; - Optionally, package and submit the package to MELPA or GNU ELPA.

;;; Code:

(require 'cl-lib)
(require 'gptel-context)
(require 'transient)

(defconst gptel-context-store-default-file
  (concat (file-name-as-directory default-directory) ".gptel-context.el")
  "Default filename for saving and loading gptel context in the current directory.")

(defun gptel-context-store--serialize (alist)
  "Serialize `gptel-context--alist` ALIST to a portable structure.
Elements representing buffer regions become (FILE START END),
files remain as (FILE) or (FILE :mime TYPE)."
  (cl-loop for elt in alist
           if (bufferp (car-safe elt))
           append
           (cl-loop for ov in (cdr elt)
                    when (and (overlayp ov)
                              (buffer-file-name (car elt)))
                    collect (list (buffer-file-name (car elt))
                                  (overlay-start ov)
                                  (overlay-end ov)))
           else if (stringp (car elt))
           collect
           (if (> (length elt) 1) elt (list (car elt)))
           end))

(defun gptel-context-store--deserialize (data)
  "Restore `gptel-context--alist` from DATA read from file.
Supports elements like (FILE START END) for regions and (FILE) or (FILE :mime ...) for files."
  (cl-remove-if-not
   #'identity
   (mapcar
    (lambda (elt)
      (pcase elt
        ((and (pred (lambda (e)
                      (and (= (length e) 3)
                           (stringp (nth 0 e)) (numberp (nth 1 e)) (numberp (nth 2 e)))))
              `(,file ,start ,end))
         (when (file-exists-p file)
           (let ((buf (find-file-noselect file)))
             (with-current-buffer buf
               (list buf (make-overlay start end buf))))))
        ((and (pred (lambda (e) (stringp (car e)))) _)
         (when (file-exists-p (car elt))
           elt))
        (_ nil)))
    data)))

;;;###autoload
(defun gptel-context-store-save (&optional file)
  "Save current gptel context to FILE.
If FILE is nil, prompt user with default filename `gptel-context-store-default-file'."
  (interactive)
  (let* ((default-dir (file-name-directory (or (buffer-file-name) default-directory)))
         (default-filename ".gptel-context.el")
         (file (or file
                   (read-file-name "Save gptel context to file: "
                                   default-dir
                                   default-filename
                                   nil
                                   default-filename)))
         (file (abbreviate-file-name file)))
    (with-temp-file file
      (prin1 (gptel-context-store--serialize gptel-context--alist) (current-buffer)))
    (message "gptel context saved to %s" file)))

;;;###autoload
(defun gptel-context-store-load (&optional file merge)
  "Load gptel context from FILE.
If MERGE is non-nil or called with prefix argument `C-u`, merge with current context, else replace.
If FILE is nil, prompt user with default filename `gptel-context-store-default-file`."
  (interactive)
  (let* ((default-dir (file-name-directory (or (buffer-file-name) default-directory)))
         (default-filename ".gptel-context.el")
         (file (or file
                   (read-file-name "Load gptel context from file: "
                                   default-dir
                                   default-filename
                                   t
                                   default-filename)))
         (file (abbreviate-file-name file))
         (prefix current-prefix-arg)
         (merge (or merge prefix))
         (data (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (read (current-buffer))))
         (alist (gptel-context-store--deserialize data)))
    (unless (listp alist)
      (user-error "Invalid gptel context data in %s" file))
    (if merge
        (dolist (elt alist)
          (cl-pushnew elt gptel-context--alist :test #'equal))
      (setq gptel-context--alist alist)))
  (gptel-context--collect)
  (message "gptel context loaded from %s" file))

(defun gptel-send-no-context ()
  "Отправить содержимое текущего буфера в GPT без контекста."
  (interactive)
  (let ((gptel-context--alist nil))
    (gptel-send)))

(defun gptel-context-store--setup-transient ()
  "Add save/load commands and send-no-context to gptel transient menu."
  (with-eval-after-load 'gptel-transient
    (require 'transient)
    (transient-append-suffix 'gptel-menu 'gptel--infix-context-remove-all
      '("S" "Save context" gptel-context-store-save))
    (transient-append-suffix 'gptel-menu 'gptel--infix-context-remove-all
      '("L" "Load context" gptel-context-store-load))
    (transient-append-suffix 'gptel-menu 'gptel--infix-context-remove-all
      '("C-<return>" "Send buffer without context" gptel-send-no-context))))

;; Setup menu commands on load
(gptel-context-store--setup-transient)

(provide 'gptel-context-store)

;;; gptel-context-store.el ends here
