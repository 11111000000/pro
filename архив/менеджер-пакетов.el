;;; менеджер-пакетов.el --- Пакетный менеджер
;;; Commentary:
;;; Code:

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(require 'package)
(package-initialize)

(unless (package-installed-p 'leaf)
  (package-refresh-contents)
  (package-install 'leaf))

(custom-set-variables
 '(leaf-enable-imenu-support t))

(eval-when-compile (require 'leaf))

(provide 'менеджер-пакетов)
;;; менеджер-пакетов.el ends here
