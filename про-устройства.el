;;;; package --- Разные специфичные устройства -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun включить-гарнитуру ()
  "Включает профиль звука HFP на гарнитуре soundcore."
  (interactive)
  (message "Включаем профиль звука HFP...")
  (start-process-shell-command "soundcore hsp" nil (expand-file-name "/home/az/pro/bin/soundcore-auto.sh")))

(defun выключить-все-микрофоны ()
  "Выключает все микрофоны."
  (interactive)
  (message "Выключаем микрофоны...")
  (start-process-shell-command "mute all mic" nil (expand-file-name "/home/az/pro/bin/mute-all-mic.sh")))

;;;; Функции переферии

(defun переключить-микрофон-alsa () "Выключить микрофон." (interactive) (async-shell-command "amixer set Capture toggle" nil nil))
(defun выключить-звук () "Выключить звук." (interactive) (async-shell-command "pamixer -t" nil nil))
(defun увеличить-громкость () "." (interactive) (async-shell-command "pamixer -i 10" nil nil))
(defun уменьшить-громкость () "." (interactive) (async-shell-command "pamixer -d 10" nil nil))
(defun увеличить-яркость () "." (interactive) (async-shell-command "echo ok" nil nil))
(defun уменьшить-яркость () "." (interactive) (async-shell-command "echo ok" nil nil))
(defun переключить-тачпад () "." (interactive) (async-shell-command "xinput toggle 13" nil nil))

(global-set-key (kbd "C-c M-<f4>") 'включить-гарнитуру)

(provide 'про-устройства)
;;; про-устройства.el ends here
