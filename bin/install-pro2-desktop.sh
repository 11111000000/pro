#!/usr/bin/env bash
# Скрипт автоматического создания desktop-файла для EXWM

DM_SESSIONS="$HOME/.local/share/xsessions"
mkdir -p "$DM_SESSIONS"

cat > "$DM_SESSIONS/pro2-exwm.desktop" <<EOF
[Desktop Entry]
Name=PRO2 EXWM (Emacs)
Comment=Emacs EXWM Session
Exec=emacs --init-directory $HOME/pro2/.emacs.d
Type=Application
DesktopNames=EXWM
EOF

echo "Файл $DM_SESSIONS/pro2-exwm.desktop создан.
Перезайдите в Display Manager и выберите 'PRO2 EXWM (Emacs)' для входа."
