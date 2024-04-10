;;; тихо-применить.el --- Приглушить сообщения -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: version

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

(require 'cl-macs)

(defun тихо-применить (old-fun &rest args)
  "Применить и приглушить сообщения функции OLD-FUN с аргументами ARGS."
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect
        (apply old-fun args)
      (advice-remove 'message #'silence))))

(provide 'тихо-применить)
;;; тихо-применить.el ends here
