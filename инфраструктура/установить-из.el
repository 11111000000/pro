;;; установить-из.el --- Установка пакетов из произвольных источников -*- lexical-binding: t; -*-
;;
;; Автор: az
;; Версия: 1.0
;; Keywords: package, install, vc
;; URL: https://github.com/username/emacs.d/blob/main/инфраструктура/установить-из.el
;;
;;; Commentary:
;;
;; Утилита для установки пакетов из GitHub/GitLab и других VCS-источников
;; через package-vc-install. Обеспечивает неинтерактивную установку.
;;
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
