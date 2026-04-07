;;; про-стартовый-экран.el --- Альтернативный стартовый экран -*- lexical-binding: t; -*-
;;
;; Автор: az
;; Версия: 1.0
;; Keywords: startup, dashboard, grid, splash
;; URL: https://github.com/username/emacs.d/blob/main/интерфейс/про-стартовый-экран.el
;;
;;; Commentary:
;;
;; Этот файл настраивает альтернативный стартовый экран в Emacs, следуя принципам
;; литературного программирования: код представлен как повествование,
;; где каждая секция объясняется и логически связана с остальными.
;;
;; Почему это важно? Стартовый экран — первое впечатление от Emacs.
;; Здесь мы используем grid.el для создания красивого экрана приветствия
;; с ASCII-артом и полезной информацией. Это делает запуск Emacs приятным.
;;
;; Структура файла:
;;  0. Введение и зависимости
;;  1. Настройка grid.el
;;  2. ASCII-арт и визуальные элементы
;;  3. Команды и функции
;;  4. Финал: Provide и ends here
;;
;; Использование: Загружается через (require 'про-стартовый-экран) в init.el.
;; Опционально — требует установки grid.el.
;;
;;; Code:

(use-package grid
  :defer t 
  :ensure t
  :init
  (unless (package-installed-p 'grid)
    (package-vc-install
     '(grid
       :vc-backend Git
       :url "https://github.com/ichernyshovvv/grid.el"
       :branch "master"))))

(require 'grid)

(defvar enlight-lipsum "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.

Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.")

(defface enlight-yellow-bold
  '((t (:foreground "#cabf00" :bold t)))
  "Yellow bold face")

(defvar enlight-guix
  (propertize
   " ..                             `.
 `--..```..`           `..```..--`   
   .-:///-:::.       `-:::///:-.     
      ````.:::`     `:::.````        
           -//:`    -::-             
            ://:   -::-              
            `///- .:::`              
             -+++-:::.               
              :+/:::-                
              `-....`                "
   'face 'enlight-yellow-bold))

(defvar enlight-guix-widget
  `( :content ,(concat "\n" (propertize "Block 1" 'face 'enlight-yellow-bold)
		              "\nGUIX MANAGEMENT WIDGET\n\n")
     :width 22 :border t :align center :padding 2))

(defvar enlight-email-width
  `( :content
     ,(concat "\n" (propertize "Block 2" 'face 'enlight-yellow-bold)
	         "\nEMAIL WIDGET\n\n")
     :padding 2 :width 22 :align center :border t))

(defvar enlight-weather-width
  `( :content
     ,(concat "\n" (propertize "Block 3" 'face 'enlight-yellow-bold)
	         "\nWEATHER WIDGET\n\n")
     :padding 2 :width 22 :border t :align center))

(defvar enlight-calendar
  (progn
    (calendar)
    (diary-mark-entries)
    (prog1 (with-current-buffer (buffer-name (current-buffer))
	         (buffer-string))
      (calendar-exit))))

(use-package enlight
  :defer t 
  :custom
  (enlight-content
   (concat
    (grid-get-box `( :align center :content ,enlight-guix :width 80))
    (grid-get-row
     (list
      (grid-get-box
       (concat
	    (grid-get-box
	     `( :content
	        ,(concat
	          (grid-get-box `( :content ,(propertize "HEADER" 'face 'highlight)
			                   :width 80 :align center))
	          (grid-get-row
	           `(,enlight-guix-widget
		         "     "
		         ,enlight-email-width
		         "     "
		         ,enlight-weather-width)))
	        :width 80))
	    enlight-calendar "\n"
	    (grid-get-row
	     `(,(concat
	         (propertize "MENU" 'face 'highlight)
	         "\n"
	         (enlight-menu
	          '(("Org Mode"
		         ("Org-Agenda (current day)" (org-agenda nil "a") "a"))
		        ("Downloads"
		         ("Transmission" transmission "t")
		         ("Downloads folder" (dired "~/Downloads") "a"))
		        ("Other"
		         ("Projects" project-switch-project "p")))))
	       ,(grid-get-column
	         `(,(propertize "THINGS TO REMEMBER" 'face 'highlight)
	           (:content ,enlight-lipsum :width 50))))))))))))
(provide 'про-стартовый-экран)

;;; про-стартовый-экран.el ends here
