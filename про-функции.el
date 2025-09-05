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
Кроме пар /SEARCH/ … /REPLACE/ также удаляет блоки вида:
строка \"<OP> CREATE ...\", затем (с возможными пустыми строками)
#+begin_src ... #+end_src. Язык блока может быть любым."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (count 0))
        ;; 1) Удаляем пары /SEARCH/ ... /REPLACE/ с двумя src-блоками
        (while (re-search-forward "^[ \t]*/SEARCH/[ \t]*$" nil t)
          (let ((block-beg (match-beginning 0)))
            (forward-line 1)
            ;; Пропускаем пустые строки
            (while (and (not (eobp))
                        (looking-at "^[ \t]*$"))
              (forward-line 1))
            (if (not (looking-at "^[ \t]*#\\+begin_src\\b"))
                ;; Не наш патч — сдвигаемся на символ и продолжаем поиск
                (goto-char (1+ block-beg))
              ;; Ищем конец первого блока
              (let ((end1 (save-excursion
                            (when (re-search-forward "^[ \t]*#\\+end_src\\b.*$" nil t)
                              (point)))))
                (if (not end1)
                    (goto-char (1+ block-beg))
                  (goto-char end1)
                  (forward-line 1)
                  ;; Пропустим пустые строки
                  (while (and (not (eobp))
                              (looking-at "^[ \t]*$"))
                    (forward-line 1))
                  (if (not (looking-at "^[ \t]*/REPLACE/[ \t]*$"))
                      (goto-char (1+ block-beg))
                    (forward-line 1)
                    ;; Пропустим пустые строки
                    (while (and (not (eobp))
                                (looking-at "^[ \t]*$"))
                      (forward-line 1))
                    (if (not (looking-at "^[ \t]*#\\+begin_src\\b"))
                        (goto-char (1+ block-beg))
                      (let ((end2 (save-excursion
                                    (when (re-search-forward "^[ \t]*#\\+end_src\\b.*$" nil t)
                                      (point)))))
                        (if (not end2)
                            (goto-char (1+ block-beg))
                          ;; Удаляем целиком блок патча
                          (let* ((del-beg block-beg)
                                 (del-end (save-excursion
                                            (goto-char end2)
                                            (line-end-position)))
                                 (del-end (min (point-max) (1+ del-end))))
                            (delete-region del-beg del-end)
                            (setq count (1+ count))
                            (goto-char del-beg))))))))))))
      ;; 2) Удаляем блоки вида <OP> CREATE ... + один src-блок
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*<OP>[ \t]+CREATE\\b.*$" nil t)
        (let ((block-beg (match-beginning 0)))
          (forward-line 1)
          ;; Пропускаем пустые строки
          (while (and (not (eobp))
                      (looking-at "^[ \t]*$"))
            (forward-line 1))
          (if (not (looking-at "^[ \t]*#\\+begin_src\\b"))
              (goto-char (1+ block-beg))
            (let ((end1 (save-excursion
                          (when (re-search-forward "^[ \t]*#\\+end_src\\b.*$" nil t)
                            (point)))))
              (if (not end1)
                  (goto-char (1+ block-beg))
                ;; Удаляем целиком блок: от строки <OP> CREATE до конца #+end_src
                (let* ((del-beg block-beg)
                       (del-end (save-excursion
                                  (goto-char end1)
                                  (line-end-position)))
                       (del-end (min (point-max) (1+ del-end))))
                  (delete-region del-beg del-end)
                  (setq count (1+ count))
                  (goto-char del-beg))))))))
    (message "Удалено блоков патчей: %d" count))))

(provide 'про-функции)
;;; про-функции.el ends here
