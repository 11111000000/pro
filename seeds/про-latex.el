;;; про-latex.el --- Seed для LaTeX/Latexmk -*- lexical-binding: t -*-
;;; Commentary:
;; Поддержка LaTeX через auctex, только если найден latexmk или pdflatex.
;;; Code:

(when (or (executable-find "latexmk")
          (executable-find "pdflatex"))
  (use-package tex
    :ensure auctex
    :defer t
    :mode ("\\.tex\\'" . latex-mode))
  (message "LaTeX seed активирован."))

(provide 'про-latex)
;;; про-latex.el ends here
