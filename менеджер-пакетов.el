;;; менеджер-пакетов.el --- Пакетный менеджер
;;; Commentary:
;;; Code:

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(require 'package)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(require 'package-vc)

(cl-defun установить-из-репы (&key (fetcher "github") repo name rev backend)
  "Установите пакет с удаленного компьютера, если он еще не установлен. Это тонкая оболочка над package-vc-install, чтобы сделать неинтерактивное использование более эргономичным. Принимает следующее именованные аргументы:

- FETCHER источник пакета (например, \"gitlab\").
  Если не указан, то \"github\".

- REPO должно быть имененем репозитария (например, \"slotThe/arXiv-citation\".

- NAME, REV, и BACKEND все как в `package-vc-install'"
  (let* ((url (format "https://www.%s.com/%s" fetcher repo))
         (iname (when name (intern name)))
         (pac-name (or iname (intern (file-name-base repo)))))
    (unless (package-installed-p pac-name)
      (package-vc-install url iname rev backend))))


(provide 'менеджер-пакетов)
;;; менеджер-пакетов.el ends here

