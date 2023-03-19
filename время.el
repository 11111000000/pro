;;; время.el --- Время и календарь -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: 1.0.0
;; Package-Requires: (dependencies)
;; Homepage: dobro.ru
;; Keywords: dobro


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

;; Время и календарь

;;; Code:

(defun set-calendar-font ()
  (set (make-local-variable 'buffer-face-mode-face) 'fixed-pitch)
  (buffer-face-mode t))

(use-package calendar
  :hook ((calendar-mode . set-calendar-font)))

(provide 'время)

;;; время.el ends here
