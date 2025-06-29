;;; про-игры.el --- Игры -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)
;;; Commentary:
;; Игры
;;; Code:

;; chess - шахматы
(use-package chess
  :defer t
  :ensure t
  :commands (chess-game-mode chess-board-mode chess-board)
  :config)

(use-package pygn-mode
  :ensure t
  :defer t)

;; Выполение боков вида
;; #+begin_src chess :file startpos.svg :notation pgn
;; 1. e4 e5
;; 2. d4 d6
;; 3. dxe5 dxe5
;; 4. Qxd8 Kxd8
;; 5. Nf3 f5
;; 6. Nxe5 fxe4
;; #+end_src


(defun org-babel-execute:chess (body params)
  "Execute a block of Chess code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((output-file (concat (file-name-sans-extension (buffer-file-name)) (format "_%s_chess_output.svg" (format-time-string "%Y-%m-%d_%H-%M-%S")) ))
        (notation (cdr (assq :notation params)))
        (extension (if (equal notation "fen") ".fen" ".pgn"))
        (notation-file (make-temp-file "chess-notation" nil extension))
        (cmd (format "python ~/configs/elchess.py %s %s %s" notation-file output-file notation)))
    (with-temp-buffer
      (insert body)
      (write-file notation-file))
    (shell-command cmd)
    (org-babel-result-to-file (file-name-nondirectory output-file))))


(setq org-babel-default-header-args:chess
     '((:results . "raw")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((chess . t)))



(provide 'про-игры)
;;; про-игры.el ends here
