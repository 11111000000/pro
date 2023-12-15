;;; код-на-lisp-русификация.el --- Русификация Emacs LISP
;;; Commentary:
;;  Русификация ЛИСП
;;; Code:

(use-package prettify-utils
  :init (установить-из :repo "Ilazki/prettify-utils.el"))

(defun добавить-текущий-символ-в-перевод ()
  "Добавить текущий символ в перевод."
  (interactive "P"))

(defun русификация-emacs-lisp ()
  "Русификация Emacs LISP."
  (interactive)
  (setq prettify-symbols-alist (prettify-utils-generate
                                  ("lambda" "λ")
                                  ("t" "да")
                                  ("nil" "нет")
                                  ("setq" "уст")
                                  ("setf" "усты")
                                  ("setenv" "уст-окружение")
                                  ("setq-local" "уст-локально")
                                  ("setq-default" "уст-по-умолчанию")
                                  ("let" "для")
                                  ("let*" "для*")
                                  ("autoload" "автозагрузка")
                                  ("symbol" "символ")
                                  ("concat" "соединить")
                                  ("buffer-file-name" "имя-файла-в-буфере")
                                  ("find-file" "найти-файл")
                                  ("message" "сообщение")
                                  ("print" "печать")
                                  ("add-to-list" "добавить-в-список")
                                  ("if" "если")
                                  ("and" "и")
                                  ("or" "или")
                                  ("not" "не")
                                  ("eq" "равно")
                                  ("when" "когда")
                                  ("while" "пока")
                                  ("with" "с")
                                  ("do" "делать")
                                  ("cl-loop" "цикл")
                                  ("for" "для")
                                  ("in" "из")
                                  ("collect" "собрать")
                                  ("into" "в")
                                  ("finally" "наконец")
                                  ("return" "вернуть")
                                  ("dotimes" "делать-раз")
                                  ("dolist" "по-списку")
                                  ("cons" "ячейка")
                                  ("car" "голова")
                                  ("cdr" "хвост")
                                  ("pop" "достать")
                                  ("push" "засунуть")
                                  ("sort" "сортировать")
                                  ("max" "максимум")
                                  ("equal" "равно")
                                  ("intern" "символ-из-строки")
                                  ("boundp" "это-определено")
                                  ("fboundp" "функция-определена")
                                  ("bound-and-true-p" "это-определено-и-истинно")
                                  ("numberp" "это-число")
                                  ("unless" "если-не")
                                  ("progn" "выполнить")
                                  ("defun"  "функция")
                                  ("require"  "загрузить")
                                  ("defvar" "переменная")
                                  ("defvar-local" "локальная-переменная")
                                  ("defconst" "константа")
                                  ("defgroup" "группа")
                                  ("defalias" "синоним")
                                  ("apply" "применить")
                                  ("eval" "вычислить")
                                  ("eval-and-compile" "вычислить-и-скомпилить")
                                  ("eval-after-load" "вычислить-после-загрузки")
                                  (":name" ":имя")
                                  (":class-decl" ":класс")
                                  (":prototype" ":прототип")
                                  (":contexts" ":контексты")
                                  (":framework" ":фреймворк")
                                  (":link" ":ссылка")
                                  (":group" ":группа")
                                  (":type" ":тип")
                                  ("defcustom" "настройка")
                                  ("defadvice" "обработчик")
                                  ("defface" "фейс")
                                  ("delete" "удалить")
                                  ("mapc" "по-списку")
                                  ("mapcar" "из-списка")
                                  ("interactive" "интерактивная")
                                  ("load-library" "загрузить-библиотеку")
                                  ("cond" "выбор")
                                  ("add-hook" "привязать-хук")
                                  ("run-hooks" "выполнить-хуки")
                                  ("error" "ошибка")
                                  ("bind-keys*" "привязать-клавиши*")
                                  ("global-set-key" "глобальная-клавиша")
                                  ("define-key" "привязать-клавишу")
                                  ("kbd" "сочетание")
                                  ("provide" "предоставить")
                                  ("load-path" "пути-загрузки")
                                  ("file-exists-p" "файл-существует")
                                  ("&optional" "&необязательный")
                                  ("window-system" "графический-режим")
                                  ("with-eval-after-load" "выполнить-после-загрузки")
                                  ("condition-case" "выполнить-с-обработкой")
                                  ("format" "формат")
                                  ("get-buffer" "получить-буфер")
                                  ("buffer-list" "список-буферов")
                                  ("with-current-buffer" "с-текущим-буфером")
                                  ("use-package"  "настройка-пакета")
                                  (":init" ":запуск")
                                  (":repo" ":репозитарий")
                                  (":ensure" ":установить")
                                  (":bind" ":клавиши")
                                  (":config" ":конфигурация")
                                  (":disabled" ":выключено")
                                  (":after" ":после")
                                  (":defer" ":отложить")
                                  (":custom" ":настройки")
                                  (":if" ":если")
                                  (":hook" ":хуки")
                                  (":map" ":раскладка")
                                  (":requires" ":требует")
                                  (":require" ":требует")
                                  (":global" ":глобально")
                                  ("current-time-string" "текущее-время-строкой")
                                  ("set-language-environment" "установить-языковое-окружение")
                                  ("prefer-coding-system" "предпочитать-кодировку")
                                  ("set-default-coding-systems" "установить-кодировки-по-умолчанию")
                                  ("set-terminal-coding-system" "установить-кодировку-терминала")
                                  ("set-keyboard-coding-system" "установить-кодировку-клавиатуры")
                                  ("set-window-margins" "установить-поля-окна")
                                  ("global-unset-key" "глобально-сбросить-клавишу")
                                  ("window-width" "ширина-окна")
                                  ("window-list" "список-окон")
                                  ("minibuffer-window" "окно-минибуфера")
                                  ("toggle-truncate-lines" "переключить-перенос-строк")
                                  ("visual-line-mode" "визуальные-строки")
                                  ("truncate-lines" "разрывать-строки")
                                  ("truncate-partial-width-windows" "разрывать-строки-в-разделённых-окнах")
                                  ("longlines-show-hard-newlines" "показывать-жёсткие-переносы")
                                  ("line-move-visual" "перемещаться-по-визуальным-строкам")
                                  ("global-prettify-symbols-mode" "глобальный-режим-преображения-символов")
                                  ("prettify-symbols-unprettify-at-point" "показывать-оригинальные-символы-под-курсором")
                                  ("scroll-bar-mode" "режим-полос-прокрутки")
                                  ("tool-bar-mode" "режим-панели-инструментов")
                                  ("menu-bar-mode" "режим-панели-меню")
                                  ("enable-recursive-minibuffers" "включить-рекурсивные-минибуферы")
                                  ("global-auto-revert-mode" "глобальный-режим-обновления")
                                  ("global-auto-revert-non-file-buffers" "глобально-обновлять-не-файловые-буферы")
                                  ("auto-revert-verbose" "авто-обновлять-молча")
                                  ("display-buffer-alist" "список-отображения-буферов")
                                  ("async-shell-command-buffer" "асинхронный-буфер-команд-оболочки")
                                  ("rename-buffer" "переименовать-буфер")
                                  ("scroll-conservatively" "скроллить-консервативно")
                                  ("goto-char" "на-символ")
                                  ("elt" "элемент")
                                  ("assoc" "среди")
                                  ("with-temp-buffer" "во-временном-буфере")
                                  ("string-match" "начало-подстроки")
                                  ("start-process-shell-command" "запустить-процесс-в-оболочке")
                                  ("file-name-directory" "директория-файла")
                                  ("locate-library" "найти-библиотеку")
                                  ("funcall-interactively" "запустить-интераткивно")
                                  ("erase-buffer" "очистить-буфер")
                                  ("insert-file" "вставить-файл")
                                  ("symbolp" "это-символ")
                                  ("functionp" "это-функция")
                                  ("length" "длина")
                                  ("text-properties-at" "текстовые-свойства-в")
                                  ("replace-regexp-in-string" "заменить-регулярку-в-строке")
                                  ("face" "фасад")
                                  ("quote" "кавычка")
                                  ("remove-hook" "удалить-хук")
                                  ("run-with-timer" "запустить-с-таймером")
                                  ("cancel-timer" "выключить-таймер")
                                  ("current-column" "текущий-столбец")
                                  ("insert" "вставить")
                                  ("current-message" "текущее-сообщение")
                                  ("string-replace" "заменить-строку")
                                  ("seq-reduce" "редукция-последовательности")
                                  ("pop-to-buffer" "поднять-буфер")
                                  ("call-process-shell-command" "вызвать-процесс-команду-оболочки")
                                  ("replace-regexp" "заменить-регуляркой")
                                  ("with-temp-file" "во-временном-файле")
                                  ("match-string" "найденая-строка")
                                  ("re-search-forward" "регулярка-вперёд")
                                  ("point-min" "точка-мин")
                                  ("insert-file-contents" "вставить-содержимое-файла")
                                  ("substring" "подстрока")
                                  ("substring-no-properties" "подстрока-без-свойств")
                                  ("bufferp" "это-буфер")
                                  (":height" ":высота")
                                  ("major-mode" "мажорный-мод")
                                  ("alist-get" "поле-объекта")
                                  ("add-face-text-property" "добавить-текстовое-свойство")
                                  ("buffer-name" "имя-буфера")
                                  ("current-buffer" "текущий-буфер")
                                  ("buffer-live-p" "это-живой-буфер")
                                  ("string-equal" "строки-равны")
                                  ("selected-window" "выбранное-окно")
                                  ("delete-window" "удалить-окно")
                                  ("switch-to-buffer-other-window" "переключиться-на-буфер-в-другом-окне")
                                  ("set-buffer" "выбрать-буфер")
                                  ("visible-bell" "визуальная-сигнализация")
                                  ("sleep-for"  "спать-сек")
                                  ("save-restriction" "сохраняя-ограничения")
                                  ("save-excursion" "сохраняя-положение")
                                  ("widen" "расширить")
                                  ("ignore-errors" "игнорируя-ошибки")
                                  ("forward-line" "вперёд-линию")
                                  ("forward-char" "вперёд-символ")
                                  ("point-marker" "маркер-точки")
                                  ))
  (prettify-symbols-mode 1))


(add-hook 'emacs-lisp-mode-hook 'русификация-emacs-lisp)
(provide 'код-на-lisp-русификация)
;;; код-на-lisp-русификация.el ends here

