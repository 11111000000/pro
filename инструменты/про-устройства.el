;;; про-устройства.el --- Управление устройствами в Emacs -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: device, audio, microphone, headset, brightness
;; URL: https://github.com/username/emacs.d/blob/main/инструменты/про-устройства.el
;;
;;; Commentary:
;;
;; Этот файл настраивает управление устройствами в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Emacs может интегрироваться с системными устройствами:
;; управлять звуком, микрофоном, яркостью, гарнитурой. Это делает Emacs
;; центром управления рабочим окружением.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Управление звуком и гарнитурой
;;  2. Управление микрофонами
;;  3. Яркость и другие функции
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-устройства) в init.el.
;; Требует соответствующих shell-скриптов в ~/pro/bin/.
;;
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
