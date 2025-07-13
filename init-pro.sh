#!/usr/bin/env bash
# Скрипт для создания минимальной структуры "ПРО" в каталоге PRO_DIR (по умолчанию ./pro)
# Развёртывание: bash ./init-pro.sh

set -e

PRO_DIR="${1:-./}"

echo "Создаём директорию PRO: $PRO_DIR"
mkdir -p "$PRO_DIR"

# 1. Структура каталогов
for d in bin etc seeds; do
  mkdir -p "$PRO_DIR/$d"
done

# 2. .gitignore, README, основные org/el файлы
cat > "$PRO_DIR/.gitignore" <<EOF
*.elc
auto-save-list/
undo/
history/
.history
*~
.cache/
seeds/
EOF

cat > "$PRO_DIR/README.org" <<EOF
#+TITLE: ПРО: Персональное Рабочее Окружение (минимальная версия)
- На базе Emacs+Guix+EXWM, структура: отдельный модуль по задаче.
- Путь: простота, воспроизводимость, Дао-пустота.
- Развёртывание: bash ./init-pro.sh && emacs --init-directory ./pro/.emacs.d

#+BEGIN_SRC bash
guix shell -m manifest.scm --emacs
#+END_SRC

--- Модули лежат в корне каталога, seeds/ для расширений ---
EOF

# 3. Простейший manifest.scm для Guix shell/dev environment
cat > "$PRO_DIR/manifest.scm" <<EOF
(specifications->manifest
  (list "emacs"
        "emacs-exwm"
        "emacs-use-package"
        "emacs-magit"
        "git"
        "coreutils"
        "fontconfig"
        "findutils"))
EOF

# 4. Стартовый early-init.el (простота, Дао-минимализм)
cat > "$PRO_DIR/early-init.el" <<EOF
;;; early-init.el --- Стартовый ранний init для ПРО -*- lexical-binding: t -*-
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-startup-message t
      inhibit-startup-screen t
      frame-resize-pixelwise t
      package-enable-at-startup nil)
(push '(font . "DejaVu Sans Mono-12") default-frame-alist)
;;; early-init.el ends here
EOF

# 5. Минималистичный init.el — только необходимое, всё по модулям, пустота приветствуется
cat > "$PRO_DIR/init.el" <<EOF
;;; init.el --- Минимальный стартовый init ПРО -*- lexical-binding: t -*-
(add-to-list 'load-path ".")
(require 'про-менеджер-пакетов)
(require 'про-внешний-вид)
(require 'про-шрифты)
(require 'про-код)
(require 'про-графическую-среду)
;;; init.el ends here
EOF

# 6. Простейшие модульные .el файлы (задающиеся как семена для роста системы)

cat > "$PRO_DIR/про-менеджер-пакетов.el" <<EOF
;;; про-менеджер-пакетов.el --- Менеджер пакетов, use-package -*- lexical-binding: t -*-
(require 'package)
(setq package-archives
      '((\"melpa\" . \"https://melpa.org/packages/\")
        (\"gnu\" . \"https://elpa.gnu.org/packages/\")
        (\"nongnu\" . \"https://elpa.gnu.org/nongnu/\")))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(provide 'про-менеджер-пакетов)
EOF

cat > "$PRO_DIR/про-внешний-вид.el" <<EOF
;;; про-внешний-вид.el --- Минимализм интерфейса -*- lexical-binding: t -*-
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(setq visible-bell nil ring-bell-function 'ignore)
(provide 'про-внешний-вид)
EOF

cat > "$PRO_DIR/про-шрифты.el" <<EOF
;;; про-шрифты.el --- Шрифты -*- lexical-binding: t -*-
(set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 120)
(provide 'про-шрифты)
EOF

cat > "$PRO_DIR/про-код.el" <<EOF
;;; про-код.el --- Пример базового развёртывания языка -*- lexical-binding: t -*-
(use-package prog-mode :ensure nil)
(use-package magit :ensure t :defer t)
(provide 'про-код)
EOF

cat > "$PRO_DIR/про-графическую-среду.el" <<EOF
;;; про-графическую-среду.el --- Минимальный EXWM Дао -*- lexical-binding: t -*-
(when (featurep 'exwm)
  (require 'exwm)
  (setq exwm-workspace-number 1)
  (exwm-enable))
(provide 'про-графическую-среду)
EOF

# 7. Создать минимальный Emacs init-directory для запуска (используем symlink при желании)
EMACS_D="$PRO_DIR/.emacs.d"
mkdir -p "$EMACS_D"
ln -sf ../early-init.el "$EMACS_D/early-init.el"
ln -sf ../init.el "$EMACS_D/init.el"

echo "Структура ПРО готова!"
echo "Теперь можете войти в окружение с помощью:"
echo "  guix shell -m $PRO_DIR/manifest.scm -- emacs --init-directory $EMACS_D"
echo "Для запуска просто Emacs с этим init:"
echo "  emacs --init-directory $EMACS_D"
echo "Добавляйте новые функции через seeds/ или модули по мере роста ПРО."
EOF
