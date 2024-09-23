;;; про-игры.el --- Игры -*- lexical-binding: t -*-
;; Автор: Пётр (11111000000@email.com)
;;; Commentary:
;; Игры
;;; Code:

;; chess - шахматы
(use-package chess
  :ensure t
  :commands (chess-game-mode chess-board-mode chess-board)
  :config)

(provide 'про-игры)
;;; про-игры.el ends here
