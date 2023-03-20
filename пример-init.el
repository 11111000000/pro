;;; пример-init.el --- Пример init.el
;;; Commentary:

;; Этот файл можно скопировать в ~/.emacs.d/init.el
;; или подключить прямо из него так:
;; ;;; ~/emacs.d/init.el
;; (add-to-list 'load-path "~/Добро")
;; (require 'пример-init)

;;; Code:

;; (add-to-list 'load-path "~/Добро")

(require 'загрузить)

(загрузить 'оптимизация-производительности)
(загрузить 'менеджер-пакетов)
(загрузить 'пароли-и-шифрование)
(загрузить 'история)
(загрузить 'внешний-вид)
(загрузить 'вкладки)
(загрузить 'управление-окнами)
(загрузить 'шрифты)
(загрузить 'цвета-дао)
(загрузить 'редактор)
(загрузить 'режим-бога)
(загрузить 'автодополнение-строк)
(загрузить 'доска)
(загрузить 'быстрый-доступ)
(загрузить 'файлы-и-папки)
(загрузить 'организация)
(загрузить 'терминалы)
(загрузить 'программирование)
(загрузить 'управление-проектами)
(загрузить 'организация)
(загрузить 'программирование-на-lisp)
(загрузить 'русификация-emacs-lisp)
(загрузить 'программирование-на-javascript)
(загрузить 'словари-и-перевод)
(загрузить 'интернет-общение)
(загрузить 'интернет-сервисы)
(загрузить 'интернет-новости)
(загрузить 'внешние-мониторы-2)
(org-babel-load-file "/home/az/Добро/глобальные-сочетания-клавиш.org")
(загрузить 'графическая-среда)
(загрузить 'инструменты)
(загрузить 'справка)


(provide 'пример-init)

;;; пример-init.el ends here
