#!/bin/sh

set -eu

script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
log_dir="${EMACS_STARTUP_LOG_DIR:-$repo_root/.agent-shell/emacs}"
log_file="${EMACS_STARTUP_LOG_FILE:-$log_dir/emacs-startup.log}"

mkdir -p "$log_dir"

exec >>"$log_file" 2>&1

printf '%s\n' "[$(date '+%F %T%z')] exwm-start: begin"
printf '%s\n' "[$(date '+%F %T%z')] exwm-start: log_file=$log_file"
printf '%s\n' "[$(date '+%F %T%z')] exwm-start: pwd=$(pwd) user=$USER display=${DISPLAY:-unset} xdg_session=${XDG_SESSION_TYPE:-unset} desktop_session=${DESKTOP_SESSION:-unset}"

xset -b

xhost +SI:localuser:$USER

wmname LG3D

export _JAVA_AWT_WM_NONREPARENTING=1

export QT_QPA_PLATFORMTHEME=qt5ct
export XMODIFIERS=@im=exwm-xim
export GTK_IM_MODULE=xim
export QT_IM_MODULE=xim
export CLUTTER_IM_MODULE=xim

export LSP_USE_PLISTS=true

setxkbmap -option 'caps:ctrl_modifier,grp:shifts_toggle,grp_led:caps'

xsetroot -cursor_name left_ptr

export VISUAL=emacsclient
export EDITOR="$VISUAL"

# Prefer a real init.el, but fall back to the repo sample config.
if [ -f "$HOME/.emacs.d/init.el" ]; then
  EMACS_INIT="$HOME/.emacs.d/init.el"
else
  EMACS_INIT="$HOME/pro/sample-init.el"
fi
printf '%s\n' "[$(date '+%F %T%z')] exwm-start: EMACS_INIT=$EMACS_INIT"

#/usr/local/bin/emacs --daemon

export EMACS_STARTUP_LOG_DIR="$log_dir"
export EMACS_STARTUP_LOG_FILE="$log_file"

printf '%s\n' "[$(date '+%F %T%z')] exwm-start: exec emacs --load $EMACS_INIT"

exec dbus-launch --exit-with-session emacs --load "$EMACS_INIT"
