;;; про-менеджер-пакетов.el --- Пакетный менеджер -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl-lib)
(require 'subr-x)

(setq-default package-archives
              '(("melpa" . "https://melpa.org/packages/")
                ("gnu" . "https://elpa.gnu.org/packages/")
                ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(require 'установить-из)

(setq package-install-upgrade-built-in t)

(defun pro/package-upgrade-async (&optional _prefix)
  "Обновить все пакеты в полностью отдельном Emacs-процессе (-Q --batch).
Лог в буфере *Package Upgrade Log*. Без вопросов (y/n), только прогресс и ошибки.
Лог выводится поточно (line-buffered) при помощи stdbuf, при его отсутствии — через PTY.
Дополнительно: устойчивость к ошибкам refresh, GC-тюнинг, тайминг выполнения, код выхода."
  (interactive "P")
  (let* ((buf (get-buffer-create "*Package Upgrade Log*"))
         (start-time (float-time))
         ;; На случай, если текущий Emacs запущен через emacsclient — выбираем «настоящий» emacs.
         (emacs (or (and (not (string-match-p "emacsclient\\'" invocation-name))
                         (expand-file-name (concat invocation-directory invocation-name)))
                    (executable-find "emacs")
                    (expand-file-name (concat invocation-directory invocation-name))))
         (stdbuf (or (executable-find "stdbuf") (executable-find "gstdbuf"))) ; macOS coreutils: gstdbuf
         ;; Фиксируем текущие пути/архивы, чтобы дочерний -Q работал с тем же каталогом пакетов.
         (parent-package-user-dir (expand-file-name package-user-dir))
         (parent-package-archives  (cl-copy-list package-archives))
         (parent-quickstart (and (boundp 'package-quickstart) package-quickstart))
         (parent-upgrade-built-in (and (boundp 'package-install-upgrade-built-in)
                                       package-install-upgrade-built-in))
         ;; Lisp, который выполнится ТОЛЬКО в дочернем Emacs (-Q --batch)
         (batch-form
          `(progn
             (setq noninteractive t
                   inhibit-message t
                   url-show-status t       ;; Показывать сетевой прогресс (Contacting host ...)
                   ;; Ускоряем batch-запуск.
                   gc-cons-threshold most-positive-fixnum
                   url-queue-timeout 60
                   url-request-timeout 60)
             (require 'package)
             (require 'cl-lib)
             ;; Не let-связываем package-archives до require, чтобы избежать
             ;; «Defining as dynamic an already lexical var».
             (setq package-user-dir ,parent-package-user-dir
                   package-archives ',parent-package-archives
                   package-quickstart ,(if parent-quickstart t nil)
                   package-install-upgrade-built-in ,(if parent-upgrade-built-in t nil))
             (package-initialize)
             (defun vlog (fmt &rest args)
               (princ (format-time-string "%H:%M:%S "))
               (princ (apply #'format (concat fmt "\n") args)))
             (vlog "== Emacs %s (isolated -Q), package-user-dir=%s"
                   emacs-version package-user-dir)
             (condition-case e
                 (progn
                   (vlog "== Refreshing package archives...")
                   (package-refresh-contents))
               (error
                (vlog "!! refresh failed: %s" (error-message-string e))))
             (let* ((installed (mapcar #'car package-alist))
                    (ok 0) (fail 0) (skipped 0))
               ;; Зафиксировать выбранные пакеты, чтобы autoremove не предлагал удалить всё.
               (setq package-selected-packages installed)
               (vlog "== Installed packages: %d" (length installed))
               (dolist (p installed)
                 (let* ((inst (car (alist-get p package-alist)))
                        (avail (car (alist-get p package-archive-contents))))
                   (condition-case e
                       (if (and inst avail
                                (version-list-< (package-desc-version inst)
                                                (package-desc-version avail)))
                           (progn
                             (vlog "-> %s: %s -> %s"
                                   p
                                   (package-version-join (package-desc-version inst))
                                   (package-version-join (package-desc-version avail)))
                             ;; Надёжное обновление для Emacs 29/30 без интерактивных вопросов:
                             ;; ставим точное описание пакета из архивов.
                             (package-install avail t)
                             (setq ok (1+ ok))
                             (vlog "<- %s: upgraded" p))
                         (setq skipped (1+ skipped))
                         (vlog "== %s: up-to-date" p))
                     (error
                      (setq fail (1+ fail))
                      (vlog "!! %s: %s" p (error-message-string e))))))
               (condition-case e
                   (let ((package-delete-prompt nil))
                     ;; Удалять бесхозные пакеты без вопросов.
                     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                               ((symbol-function 'y-or-n-p)   (lambda (&rest _) t)))
                       (package-autoremove))
                     (vlog "== autoremove: done"))
                 (error (vlog "!! autoremove: %s" (error-message-string e))))
               (when (and package-quickstart (fboundp 'package-quickstart-refresh))
                 (condition-case e
                     (progn (package-quickstart-refresh)
                            (vlog "== quickstart-refresh: done"))
                   (error (vlog "!! quickstart-refresh: %s"
                                (error-message-string e)))))
               (vlog "== Finished: %d upgraded, %d failed, %d unchanged" ok fail skipped)))))
    ;; Подготовка буфера лога
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Spawning isolated background upgrade at %s...\n\n"
                        (current-time-string))))
      (special-mode))
    ;; Сборка команды и типа соединения для поточного вывода.
    (let* ((use-stdbuf (and stdbuf t))
           (connection (if use-stdbuf 'pipe 'pty))
           (cmd (if use-stdbuf
                    (list stdbuf "-oL" "-eL" emacs "-Q" "--batch" "--eval" (prin1-to-string batch-form))
                  (list emacs "-Q" "--batch" "--eval" (prin1-to-string batch-form)))))
      (let ((process-adaptive-read-buffering nil)) ; минимальная задержка чтения
        (make-process
         :name "package-upgrade-async"
         :buffer buf
         :stderr (when (eq connection 'pipe) buf) ;; для pty stderr сольётся в stdout
         :coding 'utf-8
         :noquery t
         :connection-type connection
         :command cmd
         :filter
         (lambda (_proc chunk)
           ;; Вставляем сразу, убираем \r, скроллим вниз.
           (with-current-buffer buf
             (let ((inhibit-read-only t))
               (goto-char (point-max))
               (insert (replace-regexp-in-string "\r" "\n" chunk))
               (when (get-buffer-window (current-buffer))
                 (goto-char (point-max))))))
         :sentinel
         (lambda (proc event)
           (when (memq (process-status proc) '(exit signal))
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (goto-char (point-max))
                 (insert (format "\nDone (status %s, %.2fs): %s"
                                 (condition-case nil (process-exit-status proc) (error "n/a"))
                                 (- (float-time) start-time)
                                 (string-trim event)))))
             (run-at-time
              0 nil
              (lambda ()
                (when (y-or-n-p "Перезагрузить пакеты? y/n ")
                  (let ((reloaded (pro/reload-all-packages)))
                    (with-current-buffer buf
                      (let ((inhibit-read-only t))
                        (goto-char (point-max))
                        (insert (format "\nReloaded %d package files in current session." reloaded))))))))))))
      (unless (get-buffer-window buf t)
        (display-buffer buf)))))

(defalias 'обновить-пакеты-в-фоне #'pro/package-upgrade-async)

(defun pro/reload-all-packages ()
  "Перезагрузить все загруженные файлы пакетов из package-user-dir в текущей сессии.
Возвращает количество успешно перезагруженных файлов."
  (interactive)
  (let* ((dir (file-name-as-directory (expand-file-name package-user-dir)))
         (count 0)
         (load-prefer-newer t)
         (gc-cons-threshold most-positive-fixnum))
    ;; Обновляем load-path/активацию пакетов на случай новых версий.
    (package-initialize)
    ;; Перезагружаем все уже загруженные из ELPA/уложенные в load-history файлы.
    (dolist (entry load-history count)
      (let ((file (car entry)))
        (when (and (stringp file)
                   (string-prefix-p dir (expand-file-name file)))
          (let ((lib (file-name-sans-extension (file-name-nondirectory file))))
            (condition-case _
                (progn
                  ;; Грузим по имени библиотеки, чтобы подтянулась новая версия из load-path.
                  (load lib nil 'nomessage)
                  (setq count (1+ count)))
              (error nil))))))))

(provide 'про-менеджер-пакетов)
;;; про-менеджер-пакетов.el ends here.
