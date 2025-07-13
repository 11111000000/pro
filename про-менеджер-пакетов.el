;;; про-менеджер-пакетов.el --- Менеджер пакетов, use-package -*- lexical-binding: t -*-
(require 'package)
(setq package-archives
      '((\"melpa\" . \"https://melpa.org/packages/\")
        (\"gnu\" . \"https://elpa.gnu.org/packages/\")
        (\"nongnu\" . \"https://elpa.gnu.org/nongnu/\")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(provide 'про-менеджер-пакетов)
