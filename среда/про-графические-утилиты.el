;;; про-графические-утилиты.el --- Графические утилиты EXWM -*- lexical-binding: t -*-

;;; Commentary:
;; Этот файл содержит прикладные утилиты для EXWM: скриншоты и скринкасты.
;;
;; Структура файла:
;; 1. Скриншоты: выделенная область и весь экран.
;; 2. Скринкаст: запись экрана в ffmpeg с управлением процессом.

;;; Code:

;;;; 1. Скриншоты
;; Утилиты для захвата экрана и отправки результата в copyq.

(defun скриншот-области ()
  "Снять скриншот выделенной области и отправить в буфер обмена (copyq).
Сохраняет файлы в ~/Скриншоты/."
  (interactive)
  (let ((dir (expand-file-name "~/Скриншоты/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (start-process-shell-command
     "scrot-area" nil
     (format "scrot -s '%s%%Y-%%m-%%d_%%H.%%M.%%S.png' -e 'copyq write image/png - < $f && copyq select 0'"
             dir))))

(defun скриншот ()
  "Сделать скриншот всего экрана и отправить в copyq.
Сохраняет файлы в ~/Скриншоты/."
  (interactive)
  (sit-for 1)
  (let ((dir (expand-file-name "~/Скриншоты/")))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (start-process-shell-command
     "scrot-full" nil
     (format "scrot -d 3 '%s%%Y-%%m-%%d-%%H-%%M_$wx$h.png' -e 'copyq write image/png - < $f && copyq select 0'"
             dir))))

;;;; 2. Скринкаст
;; Запись экрана в совместимый MP4-контейнер.

(defun скринкаст (&optional duration)
  "Запустить скринкаст экрана с помощью ffmpeg.
Файл сохраняется в ~/Скринкасты/."
  (interactive "P")
  (let* ((dir (expand-file-name "~/Скринкасты/"))
         (_ (unless (file-directory-p dir)
              (make-directory dir t)))
         (fname (read-string "Имя скринкаста (без расширения): "
                             (format-time-string "%Y-%m-%d_%H-%M-%S")))
         (path (expand-file-name (concat fname ".mp4") dir))
         (log-buf (get-buffer-create (format "*скринкаст %s*" fname)))
         (size (string-trim
                (shell-command-to-string
                 "xrandr | grep '*' | head -n1 | awk '{print $1}'")))
         (display (or (getenv "DISPLAY") ":0"))
         (args (append
                (list
                 "-video_size" size
                 "-framerate" "30"
                 "-f" "x11grab"
                 "-i" display
                 "-f" "lavfi"
                 "-i" "anullsrc=channel_layout=stereo:sample_rate=48000"
                 "-c:v" "libx264"
                 "-pix_fmt" "yuv420p"
                 "-profile:v" "baseline"
                 "-level:v" "3.1"
                 "-preset" "faster"
                 "-crf" "23"
                 "-maxrate" "4500k"
                 "-bufsize" "9000k"
                 "-g" "60"
                 "-movflags" "+faststart+frag_keyframe+empty_moov"
                 "-vf" "scale=trunc(iw/2)*2:trunc(ih/2)*2"
                 "-c:a" "aac"
                 "-b:a" "128k"
                 "-ac" "2"
                 "-map" "0:v:0"
                 "-map" "1:a:0"
                 "-max_muxing_queue_size" "9999")
                (when duration (list "-t" (format "%s" duration)))
                (list "-shortest" "-f" "mp4" path)))
         (proc nil))
    (with-current-buffer log-buf (erase-buffer))
    (setq proc (apply #'start-process "ffmpeg-с-кринкаст" log-buf "ffmpeg" args))
    (set-process-query-on-exit-flag proc nil)
    (setq скринкаст-процесс proc)
    (setq скринкаст-лог-був log-buf)
    (message "Скринкаст запущен: %s (лог в %s, остановка: M-x скринкаст-стоп)"
             path (buffer-name log-buf))))

(defun скринкаст-показать-лог ()
  "Показать лог последнего процесса скринкаста (буфер ffmpeg)."
  (interactive)
  (if (and (boundp 'скринкаст-лог-був) (buffer-live-p скринкаст-лог-був))
      (pop-to-buffer скринкаст-лог-був)
    (message "Лог ещё не создан")))

(defun скринкаст-стоп ()
  "Корректно остановить запись скринкаста."
  (interactive)
  (if (and (boundp 'скринкаст-процесс) (process-live-p скринкаст-процесс))
      (progn
        (condition-case _err
            (process-send-string скринкаст-процесс "q")
          (error nil))
        (message "Скринкаст: отправлена команда остановки (q). Ждите финализации файла...")
        (run-at-time 3 nil
                     (lambda ()
                       (when (process-live-p скринкаст-процесс)
                         (ignore-errors (interrupt-process скринкаст-процесс))
                         (message "Скринкаст: SIGINT отправлен, завершаем запись..."))))
        (run-at-time 7 nil
                     (lambda ()
                       (when (process-live-p скринкаст-процесс)
                         (ignore-errors (kill-process скринкаст-процесс))
                         (message "Скринкаст: принудительно остановлен (SIGKILL).")))))
    (message "Нет активного процесса скринкаста! Используйте killall ffmpeg для остановки всех ffmpeg.")))

(provide 'про-графические-утилиты)
;;; про-графические-утилиты.el ends here
