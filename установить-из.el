;;; установить-из.el --- Установить из источника -*- lexical-binding: t -*-

;; Author: az
;; Maintainer: az
;; Version: 1.0
;; Package-Requires: (package-vc)
;; Homepage: homepage
;; Keywords: emacs

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

;; Установить из источника

;;; Code:

(require 'package-vc)

(cl-defun установить-из (&key (fetcher "github") repo name rev backend)
  "Установить пакет с удаленного компьютера, если он еще не установлен.
Это тонкая оболочка над `package-vc-install`, чтобы сделать неинтерактивное
использование более эргономичным.  Принимает аргументы:

- FETCHER источник пакета (например, \"gitlab\").
  Если не указан, то \"github\".

- REPO имя репозитария (например, \"slotThe/arXiv-citation\".

- NAME, REV, и BACKEND все как в `package-vc-install'"
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
        (iname (when name (intern name)))
        (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))

(provide 'установить-из)

;;; установить-из.el ends here
