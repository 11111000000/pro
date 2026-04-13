;;; про-ии-ввод-вывод.el --- AI input/output integrations -*- coding: utf-8; lexical-binding: t; -*-

;;; Commentary:
;; Слой AI ввода-вывода: speech, shell, workspace helpers и buffer UX.

;;; Code:

(defvar про-ии-ввод-вывод--initialized nil
  "Non-nil when the AI I/O layer has been loaded.")

(setq про-ии-ввод-вывод--initialized t)

(use-package whisper
  :init (when (fboundp 'установить-из)
          (установить-из :repo "natrys/whisper.el"))
  :custom ((whisper--ffmpeg-input-format "alsa"))
  :config
  (setq whisper-install-directory (expand-file-name "~/.emacs.d/"))
  (setq whisper-model "large")
  (setq whisper-language "ru")
  (setq whisper-translate nil)
  (setq whisper-quantize nil)
  (setq whisper-insert-text-at-point t)
  (setq whisper-recording-timeout 500)
  (setq whisper-use-threads (/ (num-processors) 1)))

(use-package aidermacs
  :ensure t
  :if (display-graphic-p)
  :config (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-use-architect-mode nil)
  (aidermacs-show-diff-after-change nil)
  :init
  (setq aidermacs-auto-commits nil))

(use-package context-navigator
  :load-path "~/Code/context-navigator/lisp"
  :custom
  (context-navigator-language 'ru)
  (context-navigator-global-key "C-c n")
  :config
  (context-navigator-mode 1))

(use-package carriage-mode
  :demand t
  :load-path "~/Code/carriage/lisp"
  :custom
  (carriage-keys-prefix "C-c e")
  (carriage-keys-prefix-alias "s-e")
  :config
  (setq carriage-mode-default-model "gptel:ai-tunnel:gpt-4.1")
  (carriage-global-mode t))

(use-package agent-shell
  :ensure t
  :preface
  (defvar-local про-ии-agent-shell--ui-restored nil
    "Non-nil when agent-shell UI was already reasserted in this buffer.")

  (defun про-ии-agent-shell--reload-after-first-turn (orig-fun &rest args)
    "Preserve visible agent-shell UI once, without rebuilding it repeatedly."
    (let ((result (apply orig-fun args)))
      (when (and (derived-mode-p 'agent-shell-mode)
                 (not про-ии-agent-shell--ui-restored))
        (setq про-ии-agent-shell--ui-restored t)
        (agent-shell-ui-mode +1)
        (agent-shell--update-header-and-mode-line))
      result))
  :bind (:map agent-shell-mode-map
              ("C-RET" . newline)
              ("M-RET" . newline)
              ("RET" . shell-maker-submit)
              ("C-g" . nil)
              ("C-с C-g" . agent-shell-interrupt)
              ("C-c C-u" . agent-shell-show-usage))
  :custom
  (agent-shell-header-style 'text)
  (agent-shell-show-config-icons t)
  (agent-shell-show-session-id nil)
  (agent-shell-show-welcome-message t)
  (agent-shell-show-busy-indicator t)
  (agent-shell-section-functions nil)
  (agent-shell-show-context-usage-indicator t)
  (agent-shell-show-usage-at-turn-end t)
  (agent-shell-thought-process-expand-by-default nil)
  (agent-shell-tool-use-expand-by-default nil)
  (agent-shell-user-message-expand-by-default nil)
  (agent-shell-prefer-viewport-interaction nil)
  (agent-shell-highlight-blocks nil)
  (agent-shell-confirm-interrupt nil)
  (agent-shell-prefer-session-resume t)
  (agent-shell-embed-file-size-limit 102400)
  (shell-maker-logging nil)
  (agent-shell-transcript-file-path-function
   (lambda ()
     (expand-file-name
      (format-time-string "%F-%H-%M-%S.md")
      (agent-shell--dot-subdir "transcripts"))))
  (agent-shell-preferred-agent-config 'opencode)
  (agent-shell-session-strategy 'prompt)
  :config
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

  (defun про-ии-agent-shell-rename-buffer-advice (orig-fun name &optional unique)
    "Keep agent-shell buffer names shortened even after later renames."
    (if (and (derived-mode-p 'agent-shell-mode)
             (stringp name)
             (string-prefix-p "OpenCode Agent" name))
        (funcall orig-fun (про-ии-agent-shell--pretty-buffer-name name) unique)
      (funcall orig-fun name unique)))

  (add-hook 'agent-shell-mode-hook #'про-ии-agent-shell-rename-buffer)
  (advice-add 'rename-buffer :around #'про-ии-agent-shell-rename-buffer-advice)
  (advice-add 'agent-shell--handle :around #'про-ии-agent-shell--reload-after-first-turn))

(provide 'про-ии-ввод-вывод)
;;; про-ии-ввод-вывод.el ends here
