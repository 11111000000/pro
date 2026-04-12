;;; про-ии-команды.el --- Команды AI-интеграции ПРО -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Интерактивные команды AI: переключение backend/model и agent-shell UX.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun pro-ai--read-with-consult-or-completing-read (prompt candidates &optional require-match initial annotate)
  "Выбрать строку из CANDIDATES через consult, если он доступен."
  (if (require 'consult nil t)
      (consult--read candidates
                     :prompt prompt
                     :require-match require-match
                     :initial initial
                     :annotate annotate)
    (completing-read prompt candidates nil require-match nil nil initial)))

(defun pro/ai-switch-backend ()
  "Интерактивный выбор и активация `gptel-backend'."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "gptel недоступен"))
  (let* ((known-backends (and (boundp 'gptel--known-backends) gptel--known-backends))
         (backends (mapcar #'car known-backends))
         (current (and (boundp 'gptel-backend) gptel-backend))
         (cur-str (or (and current (gptel-backend-name current)) "")))
    (if (null backends)
        (user-error "Нет зарегистрированных бэкендов GPTEL")
      (let ((choice (pro-ai--read-with-consult-or-completing-read
                     (format "Выберите AI backend (текущий: %s): " cur-str)
                     backends t)))
        (when (and choice (not (string-empty-p choice)))
          (setq gptel-backend (gptel-get-backend choice))
          (message "Переключено на backend: %s" choice)
          (force-mode-line-update t))))))

(defun pro/ai-switch-model ()
  "Интерактивный выбор модели из всех доступных во всех бэкендах."
  (interactive)
  (unless (require 'gptel nil t)
    (user-error "gptel недоступен"))
  (let* ((known (and (boundp 'gptel--known-backends) gptel--known-backends)))
    (when (null known)
      (user-error "Нет зарегистрированных бэкендов GPTEL"))
    (let* ((models-alist
            (cl-loop for (name . backend) in known
                     nconc (cl-loop for model in (gptel-backend-models backend)
                                    collect (let* ((model-name (gptel--model-name model))
                                                   (full-name (concat (string-trim-right name) ":" model-name))
                                                   (desc (or (get model :description) "")))
                                              (cons full-name (list :name name :model model :desc desc))))))
           (current-backend-name (and (boundp 'gptel-backend) (gptel-backend-name gptel-backend)))
           (current-model-name (and (boundp 'gptel-model) (gptel--model-name gptel-model)))
           (initial (and current-backend-name current-model-name
                         (concat current-backend-name ":" current-model-name)))
           (candidates (mapcar #'car models-alist))
           (annotate (lambda (cand)
                       (let* ((item (cdr (assoc cand models-alist)))
                              (desc (plist-get item :desc)))
                         (when (and desc (not (string-empty-p desc)))
                           (concat " — " desc)))))
           (choice (pro-ai--read-with-consult-or-completing-read
                    "Выберите модель (формат: Бэкенд:Модель): "
                    candidates t initial annotate)))
      (when choice
        (let* ((item (cdr (assoc choice models-alist)))
               (backend-name (plist-get item :name))
               (model (plist-get item :model))
               (backend (gptel-get-backend backend-name)))
          (setq gptel-backend backend
                gptel-model model)
          (message "Переключено на модель: %s (бэкенд: %s)" (gptel--model-name model) backend-name)
          (force-mode-line-update t))))))

(defun про-ии-agent-shell--pretty-buffer-name (name)
  "Return NAME with the agent-shell prefix shortened."
  (replace-regexp-in-string "\\`OpenCode Agent\\s-*" "🤖 " name))

(defun про-ии-agent-shell-rename-buffer ()
  "Normalize the initial agent-shell buffer name safely."
  (condition-case nil
      (when (and (derived-mode-p 'agent-shell-mode)
                 (string-prefix-p "OpenCode Agent" (buffer-name))
                 (boundp 'shell-maker-config)
                 shell-maker-config)
        (let ((short-name (про-ии-agent-shell--pretty-buffer-name (buffer-name))))
          (rename-buffer short-name t)))
    (error nil)))

(with-eval-after-load 'agent-shell
  (add-hook 'agent-shell-mode-hook #'про-ии-agent-shell-rename-buffer))

(provide 'про-ии-команды)
;;; про-ии-команды.el ends here
