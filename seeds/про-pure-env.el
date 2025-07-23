;;; про-pure-env.el --- Проверка чистоты окружения (Guix/Nix) -*- lexical-binding: t -*-
;;; Commentary:
;; Если окружение Emacs не собрано через Guix или Nix shell, выводит дружеское предупреждение.
;; Это необязательно, но поддерживает reproducibility!

;;; Code:

(defun про/pure-env? ()
  "Проверить, работает ли Emacs внутри Nix/Guix shell."
  (or (getenv "GUIX_ENVIRONMENT")
      (getenv "IN_NIX_SHELL")))

(unless (про/pure-env?)
  (run-at-time
   1 nil
   (lambda ()
     (display-warning
      'про-pure-env
      "Вы стартовали Emacs не в окружении Guix/Nix shell!\n
Это не запрещено, но reproducibility и совместимость могут быть не гарантированы.\n
Рекомендуется запуск:\n
  guix shell -m manifest.scm -- emacs --init-directory ./pro2/.emacs.d
  nix develop nix/manifest.nix -c emacs --init-directory ./pro2/.emacs.d
"
      :warning))))

(provide 'про-pure-env)
;;; про-pure-env.el ends here
