;;; про-организацию.el --- Org-mode и базовый GTD -*- lexical-binding: t -*-
;;; Commentary:
;; Мини-группа: org-mode Org Agenda, todo, inline images.
;;; Code:

(use-package org
  :ensure t
  :custom
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (org-todo-keywords '((sequence "TODO" "INPROG" "WAIT" "|" "DONE" "CANCELLED")))
  (org-ellipsis "…"))

(provide 'про-организацию)
;;; про-организацию.el ends here
