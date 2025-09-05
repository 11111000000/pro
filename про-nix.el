;;; про-nix.el --- Поддержка Nix в Emacs -*- lexical-binding: t -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: nix, envrc, environment
;; URL: https://example.com/про-nix
;;
;;; Commentary:
;;
;; Этот файл настраивает поддержку Nix в Emacs, следуя принципам литературного
;; программирования: код представлен как повествование, где каждая
;; секция мотивируется, объясняется и логически связывается с остальными. Мы
;; стремимся к элегантности, минимализму и производительности в лучших традициях
;; Emacs — с использованием `use-package` для декларативной конфигурации и хуков
;; для автоматизации.
;;
;; Почему это важно? Nix — мощная система управления пакетами и окружениями,
;; обеспечивающая воспроизводимость. Здесь мы интегрируем nix-mode для редактирования
;; .nix-файлов, envrc для автоматической загрузки Nix-окружений и функции для
;; перестройки системы, делая Emacs центром работы с Nix без излишеств.
;;
;; Структура файла:
;; 0. Введение и зависимости: Базовые require.
;; 1. Nix-mode: Поддержка редактирования .nix.
;; 2. Envrc: Автозагрузка окружений.
;; 3. Кастомные функции: Перестройка NixOS.
;; 4. Финал: Provide и ends here.
;;
;; Использование: Загружайте через (require 'про-nix) в вашем init.el.
;; Рекомендуется для пользователей NixOS/Guix.
;;
;; Замечания: Мы предпочитаем отложенную загрузку и минимальные глобальные изменения.

;;; Code:

;;;; 1. Nix-mode
;; Nix-mode предоставляет синтаксическую подсветку и базовые инструменты для
;; .nix-файлов. Мы задаём ассоциацию с расширением и оставляем место для
;; дополнительных хуков, мотивируя интеграцию с другими режимами (например, Lisp).

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode)
                                        ;:hook (nix-mode . lisp-interaction-mode) ;; если нужен дополнительный режим
  :config
  ;; Можно добавить дополнительные настройки тут.
  )

;;;; 2. Envrc
;; Envrc автоматически загружает Nix-окружения из .envrc или shell.nix,
;; обеспечивая seamless интеграцию с direnv. Это ключевая фича для проектов,
;; где окружение критично, связывая Emacs с внешней экосистемой.

(use-package envrc
  :ensure t

  ;; :config
  ;; (envrc-global-mode)
  )

;;;; 3. Кастомные функции
;; Эти функции расширяют базовую поддержку: настройка путей для Nix и
;; интерактивная перестройка системы. Захомментированные части — для
;; ручной настройки профилей, а основная функция упрощает rebuild,
;; выводя логи в буфер для анализа.

;; (when (file-exists-p "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh")
;;      (setenv "NIX_PROFILES"
;;              (concat "/nix/var/nix/profiles/per-user/"
;;                      (user-login-name)
;;                      "/profile:/nix/var/nix/profiles/default"))
;;      (setenv "NIX_PATH" "nixpkgs=/nix/var/nix/profiles/per-user/root/channels/nixpkgs")
;;      (let ((profile-script "/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh"))
;;        (when (file-exists-p profile-script)
;;          (shell-command (concat ". " profile-script " && env") "*Nix-env/")
;;          (with-current-buffer "*Nix-env/"
;;            (goto-char (point-min))
;;            (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)$" nil t)
;;              (setenv (match-string 1) (match-string 2))))
;;          (kill-buffer "*Nix-env/"))))


;; (setenv "PATH"
;;         (concat (expand-file-name "~/.nix-profile/bin:")
;;                 (getenv "PATH")))
;; (add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))

(defun про/пересобрать-nix ()
  "Запустить 'sudo nixos-rebuild switch' с конфигом пользователя и показать вывод в буфере *nixos-log*."
  (interactive)
  (let ((cmd (format "sudo nixos-rebuild switch -I nixos-config=%s/.config/nixos/configuration.nix"
                     (getenv "HOME")))
        (log-buf "*nixos-log*"))
    (with-current-buffer (get-buffer-create log-buf)
      (read-only-mode 0)
      (erase-buffer)
      (insert (format "Выполняем: %s\n\n" cmd))
      (read-only-mode 1))
    (let ((proc (start-process-shell-command "nixos-rebuild" log-buf cmd)))
      (set-process-sentinel
       proc
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (with-current-buffer (process-buffer proc)
             (read-only-mode 0)
             (goto-char (point-max))
             (insert (format "\n--- Завершено с кодом %d ---\n" (process-exit-status proc)))
             (read-only-mode 1))
           (display-buffer (process-buffer proc))))))
    (display-buffer log-buf)))

;;;; 4. Финал
;; Завершаем модуль, предоставляя его для require в других файлах.

(provide 'про-nix)
;;; про-nix.el ends here
