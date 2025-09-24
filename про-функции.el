;;; про-функции.el --- Полезные вспомогательные функции -*- lexical-binding: t; -*-
;;; Commentary:
;; В этом файле размещаются различные вспомогательные команды и функции,
;; которые могут использоваться в других частях про-конфига.

;;; Code:

(defun pro/current-md-to-org ()
  "Конвертировать текущий .md-файл в .org с помощью pandoc."
  (interactive)
  (let* ((md-file (buffer-file-name))
         (ext (file-name-extension md-file))
         (org-file (concat (file-name-sans-extension md-file) ".org")))
    (unless (and md-file (string= ext "md"))
      (user-error "Текущий буфер не markdown-файл"))
    (when (file-exists-p org-file)
      (unless (yes-or-no-p (format "Файл %s уже существует. Перезаписать? " org-file))
        (user-error "Операция отменена пользователем.")))
    (let ((cmd (format "pandoc -f markdown -t org '%s' -o '%s'" md-file org-file)))
      (message "Выполняется: %s" cmd)
      (shell-command cmd)
      (find-file-other-window org-file)
      (message "Конвертация завершена: %s" org-file))))

(defun pro/remove-search-replace-blocks ()
  "Удалить из текущего буфера все блоки патчей gptel-aibo.

Удаляет:
- пары /SEARCH/ … /REPLACE/ с двумя код-блоками;
- блоки вида: строка \"<OP> CREATE ...\", затем (с возможными пустыми строками)
  код-блок либо в формате Org (# +begin_src ... # +end_src), либо в формате
  ограждений из обратных кавычек (``` ... ```). Язык блока может быть любым."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (count 0))
        (let* ((skip-blanks
                (lambda ()
                  (while (and (not (eobp))
                              (looking-at "^[ \t]*$"))
                    (forward-line 1))))
               (code-block-end
                (lambda ()
                  (cond
                   ;; Org src block
                   ((looking-at "^[ \t]*#\\+begin_src\\b")
                    (when (re-search-forward "^[ \t]*#\\+end_src\\b.*$" nil t)
                      (line-end-position)))
                   ;; Markdown fence ```... (three or more backticks)
                   ((looking-at "^[ \t]*\\(=\\{3,\\}\\)[ \t]?.*$")
                    (let ((fence (match-string 1)))
                      (forward-line 1)
                      (when (re-search-forward (concat "^[ \t]*"
                                                       (regexp-quote fence)
                                                       "[ \t]*$")
                                               nil t)
                        (line-end-position))))
                   (t nil)))))
          ;; 1) Удаляем пары /SEARCH/ ... /REPLACE/ с двумя код-блоками
          (while (re-search-forward "^[ \t]*/SEARCH/[ \t]*$" nil t)
            (let ((block-beg (match-beginning 0)))
              (forward-line 1)
              (funcall skip-blanks)
              (let ((end1 (save-excursion (funcall code-block-end))))
                (if (not end1)
                    (goto-char (1+ block-beg))
                  (goto-char end1)
                  (forward-line 1)
                  (funcall skip-blanks)
                  (if (not (looking-at "^[ \t]*/REPLACE/[ \t]*$"))
                      (goto-char (1+ block-beg))
                    (forward-line 1)
                    (funcall skip-blanks)
                    (let ((end2 (save-excursion (funcall code-block-end))))
                      (if (not end2)
                          (goto-char (1+ block-beg))
                        (let ((del-beg block-beg)
                              (del-end (min (point-max) (1+ end2))))
                          (delete-region del-beg del-end)
                          (setq count (1+ count))
                          (goto-char del-beg)))))))))
          ;; 2) Удаляем блоки вида <OP> CREATE ... + один код-блок
          (goto-char (point-min))
          (while (re-search-forward "^[ \t]*<OP>[ \t]+CREATE\\b.*$" nil t)
            (let ((block-beg (match-beginning 0)))
              (forward-line 1)
              (funcall skip-blanks)
              (let ((end1 (save-excursion (funcall code-block-end))))
                (if (not end1)
                    (goto-char (1+ block-beg))
                  (let ((del-beg block-beg)
                        (del-end (min (point-max) (1+ end1))))
                    (delete-region del-beg del-end)
                    (setq count (1+ count))
                    (goto-char del-beg)))))))
        (message "Удалено блоков патчей: %d" count)))))



(provide 'про-функции)
;;; про-функции.el ends here
