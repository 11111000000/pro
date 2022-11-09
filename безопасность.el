;; * Безопасность и шифрование

;; GNU Privacy Guard (GnuPG, GPG) — свободная программа для шифрования информации и создания электронных цифровых подписей. Разработана как альтернатива PGP и выпущена под свободной лицензией.
;; (setq epg-gpg-program "gpg2")

;;(custom-set-variables '(epg-gpg-program  "/run/current-system/profile/bin/gpg")) 

;; или /home/az/.guix-profile/bin/gpg

;; Используется встроенный в EMACS GPG-агент

(setenv "GPG_AGENT_INFO" nil)

;; Ввод пароля от ключа производится ч-з минибуфер

(setq-default epa-pinentry-mode 'loopback)
(setenv "GPG_AGENT_INFO" nil)

;; Список пар логин-пароль к ресурсам - это простой текстовый файл *.authinfo*, зашифрованый GPG2:

;;(setq auth-sources '("~/.authinfo.gpg"))


;; Прозрачное шифрование файлов

(require 'epa-file)
;; (epa-file-enable)

(provide 'безопасность)
