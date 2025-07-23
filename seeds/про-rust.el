;;; про-rust.el --- Минималистичный seed Rust -*- lexical-binding: t -*-
;;; Commentary:
;; Rust-mode, автоматическое форматирование, только если rustc присутствует.
;;; Code:

(when (executable-find "rustc")
  (use-package rust-mode
    :ensure t
    :mode ("\\.rs\\'" . rust-mode)
    :hook (rust-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (setq rust-format-on-save t))))
  (use-package cargo
    :ensure t
    :hook (rust-mode . cargo-minor-mode))
  (message "Rust seed активирован."))

(provide 'про-rust)
;;; про-rust.el ends here
