;;; про-код-на-java.el --- Поддержка Java/Android в Emacs (Eglot + Gradle + утилиты) -*- lexical-binding: t; -*-
;;
;; Автор: Пётр <11111000000@email.com>
;; Версия: 1.0
;; Keywords: java, android, eglot, lsp, gradle, jdtls
;; URL: https://github.com/username/emacs.d/blob/main/разработка/про-код-на-java.el
;;
;;; Commentary:
;;
;; Минималистичная поддержка Java/Android-разработки в Emacs:
;; - Режим для .java (java-ts-mode или java-mode).
;; - LSP через Eglot + Eclipse JDT LS (jdtls).
;; - Gradle/Android-проекты: удобные команды сборки/тестов через compile.
;; - Дополнительно: gradle-mode и android-mode (если нужны команды/утилиты).
;;
;; Требования:
;; - Emacs 29+ (желательно) для java-ts-mode (иначе будет java-mode).
;; - Установленный JDT Language Server (jdtls) в PATH для LSP:
;;   например: nix, brew, apt, sdkman, или ручная установка.
;; - Для Android-CLI команд: adb/gradlew в проекте.
;;
;; Использование:
;;   (require 'про-код-на-java)
;;
;;; Code:

(require 'use-package)

;;;; 1. Java major-mode + Eglot (jdtls)
;; Основная цель — получить LSP: навигация, рефакторинг, подсказки, диагностика.
;; Eglot по умолчанию сам стартует сервер, если он известен и доступен.

(defun pro/java--project-root ()
  "Вернуть корень проекта для Java/Android (gradle/maven/git), либо nil."
  (or (locate-dominating-file default-directory "settings.gradle")
      (locate-dominating-file default-directory "settings.gradle.kts")
      (locate-dominating-file default-directory "build.gradle")
      (locate-dominating-file default-directory "build.gradle.kts")
      (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory ".git")))

(defun pro/java-eglot-ensure ()
  "Запустить Eglot для Java, только если доступен LSP-сервер."
  (interactive)
  (if (executable-find "jdtls")
      (eglot-ensure)
    (message "jdtls не найден в PATH — пропускаю eglot-ensure")))

;; Регистрируем jdtls для java-mode/java-ts-mode.
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((java-mode java-ts-mode) . ("jdtls"))))

(defun pro/java--use-ts-mode-p ()
  "Ненулевое значение, если можно безопасно использовать `java-ts-mode'.
Важно: `java-ts-mode' может быть определён, но без установленной tree-sitter
грамматики для Java он будет сигналить ошибку."
  (and (fboundp 'java-ts-mode)
       (fboundp 'treesit-language-available-p)
       (treesit-language-available-p 'java)))

(defun pro/java-mode ()
  "Открыть Java-файл в лучшем доступном major-mode.
Используем `java-ts-mode', только если доступна tree-sitter грамматика Java;
иначе откатываемся на `java-mode'."
  (interactive)
  (if (pro/java--use-ts-mode-p)
      (java-ts-mode)
    (java-mode)))

;; .java → выбираем major-mode динамически, чтобы не падать без tree-sitter.
(add-to-list 'auto-mode-alist '("\\.java\\'" . pro/java-mode))

;; Хуки для классического java-mode (из cc-mode).
(use-package cc-mode
  :defer t)

(add-hook 'java-mode-hook #'pro/java-eglot-ensure)
(add-hook 'java-mode-hook #'pro/java-android-maybe-setup-compile-command)

;; Хуки для java-ts-mode (если он вообще загрузится/будет доступен).
(with-eval-after-load 'java-ts-mode
  (add-hook 'java-ts-mode-hook #'pro/java-eglot-ensure)
  (add-hook 'java-ts-mode-hook #'pro/java-android-maybe-setup-compile-command))

;;;; 2. Gradle/Android: удобный compile-command
;; В Android/Gradle проектах хочется быстрый C-c C-c (compile) без лишних вопросов.
;; Мы локально выставляем compile-command, если видим gradlew.

(defun pro/java-android-maybe-setup-compile-command ()
  "Если рядом Gradle wrapper, настроить `compile-command' для проекта."
  (let* ((root (pro/java--project-root))
         (gradlew (and root (or (expand-file-name "gradlew" root)
                                (expand-file-name "gradlew.bat" root)))))
    (when (and root gradlew (file-exists-p gradlew))
      ;; Пытаемся использовать ./gradlew (Linux/macOS). Для Windows оставляем gradlew.bat.
      (setq-local compile-command
                  (cond
                   ((file-exists-p (expand-file-name "gradlew" root))
                    (format "cd %s && ./gradlew assembleDebug" (shell-quote-argument root)))
                   ((file-exists-p (expand-file-name "gradlew.bat" root))
                    (format "cd %s && gradlew.bat assembleDebug" (shell-quote-argument root)))
                   (t
                    (format "cd %s && gradle assembleDebug" (shell-quote-argument root))))))))

;;;; 3. Gradle-mode (опционально)
;; Удобства для build.gradle/build.gradle.kts: подсветка, команды, minor-mode.

(use-package gradle-mode
  :defer t
  :ensure t
  :mode (("build\\.gradle\\'" . gradle-mode)
         ("build\\.gradle\\.kts\\'" . gradle-mode)
         ("settings\\.gradle\\'" . gradle-mode)
         ("settings\\.gradle\\.kts\\'" . gradle-mode)))

;;;; 4. Android-mode (опционально)
;; Набор Android/ADB утилит внутри Emacs (зависит от пакета).
;; Если вы не пользуетесь — можно удалить секцию.

(use-package android-mode
  :defer t
  :ensure t
  :commands (android-start-emulator android-start-ddms android-logcat android-compile android-install))

(provide 'про-код-на-java)
;;; про-код-на-java.el ends here
