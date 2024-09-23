;;; про-менеджер-пакетов.el --- Пакетный менеджер
;;; Commentary:
;;; Code:

(require 'package)

(setq-default package-archives
                        '(("melpa" . "https://melpa.org/packages/")
                          ("gnu" . "https://elpa.gnu.org/packages/")
                          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(custom-set-variables
 '(use-package-enable-imenu-support t))

(eval-when-compile (require 'use-package))

(require 'установить-из)

(setq package-install-upgrade-built-in t)

(provide 'про-менеджер-пакетов)
;;; про-менеджер-пакетов.el ends here.
