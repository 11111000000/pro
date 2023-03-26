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

(загрузить 'оптимизация)
(загрузить 'менеджер-пакетов)
(загрузить 'пароли-и-шифрование)
(загрузить 'история)
(загрузить 'внешний-вид)
(загрузить 'цвета-дао)
(загрузить 'буферы)
(загрузить 'вкладки)
(загрузить 'шрифты)
(загрузить 'редактор)
(загрузить 'режим-бога)
(загрузить 'окна)
(загрузить 'автодополнение)
(загрузить 'доска)
(загрузить 'быстрый-доступ)
(загрузить 'файлы-и-папки)
(загрузить 'организация)
(загрузить 'терминалы)
(загрузить 'программирование)
(загрузить 'управление-проектами)
(загрузить 'организация)
(загрузить 'код-на-lisp)
(загрузить 'код-на-lisp-русификация)
(загрузить 'код-на-javascript)
(загрузить 'код-на-rust)
(загрузить 'словари-и-перевод)
(загрузить 'интернет-общение)
(загрузить 'интернет-сервисы)
(загрузить 'интернет-новости)
(загрузить 'внешние-мониторы)
(загрузить 'графическая-среда)
(загрузить 'инструменты)
(загрузить 'справка)

(org-babel-load-file "/home/az/Добро/сочетания-клавиш.org")
(delete-file "/home/az/Добро/сочетания-клавиш.el")



(provide 'пример-init)

;;; пример-init.el ends here
