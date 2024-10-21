;;; про-стартовый-экран.el --- Альтернативный стартовый экран -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

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
