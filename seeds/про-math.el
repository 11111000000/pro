;;; про-math.el --- Математическое Дао: калькулятор, формулы, calc, org-таблицы -*- lexical-binding: t -*-
;;; Commentary:
;; Математические дополнения: calc, orgtbl, inline LaTeX в Org. Всё — только если нужные компоненты доступны.
;;; Code:

;; Встроенный Emacs-калькулятор
(global-set-key (kbd "C-c c") #'calc)

;; Org-mode: рендеринг формул LaTeX inline
(with-eval-after-load 'org
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  (setq org-preview-latex-default-process 'dvisvgm)
  (setq org-highlight-latex-and-related '(latex script entities)))

;; Быстрое открытие scratch/org математической
(defun про/open-math-scratch ()
  "Открыть отдельный org-файл для математических заметок."
  (interactive)
  (find-file (expand-file-name "../org/math.org" user-emacs-directory)))
(global-set-key (kbd "C-c M") #'про/open-math-scratch)

(message "Math seed активирован (calc, Org-формулы).")

(provide 'про-math)
;;; про-math.el ends here
