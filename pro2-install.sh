#!/usr/bin/env bash
# PRO2-DAO: Скрипт быстрой установки и первого запуска
# Использование: bash ./pro2-install.sh [путь/куда/клонить]
set -e

REPO_URL="https://your-git-host/your-user/pro2.git"
DEST="${1:-$HOME/pro2}"

# 1. Установка git если нужно
if ! command -v git >/dev/null 2>&1; then
  echo "Git не найден. Установите git вручную (apt install git, guix install git, nix-env -i git)."
  exit 1
fi

# 2. Клонирование
if [ -d "$DEST" ]; then
    echo "Директория $DEST уже существует."
else
    echo "Клонирую PRO2 в $DEST ..."
    git clone "$REPO_URL" "$DEST"
fi

cd "$DEST"

# 3. Информация по дальнейшим шагам
cat <<EOF

==============================================
 PRO2 готов к воспроизводимому запуску!
==============================================

1. Вход в среду Guix/Nix:
   Guix:
     guix shell -m manifest.scm -- emacs --init-directory \$PWD/.emacs.d

   Nix:
     nix develop nix/manifest.nix -c emacs --init-directory \$PWD/.emacs.d

2. После первого запуска Emacs:
   - Все seeds будут автоматически загружены.
   - Кастомизировать окружение — просто добавьте *.el в seeds/

3. Интеграция с Display Manager (EXWM):
   — Для LightDM, GDM, SDDM добавьте файл \$HOME/.local/share/xsessions/pro2-exwm.desktop со следующим содержимым:

   [Desktop Entry]
   Name=PRO2 EXWM (Emacs)
   Comment=Emacs EXWM Session
   Exec=emacs --init-directory \$HOME/pro2/.emacs.d
   Type=Application
   DesktopNames=EXWM

   Теперь логинитесь через "PRO2 EXWM (Emacs)" при старте X11.

==============================================
EOF
