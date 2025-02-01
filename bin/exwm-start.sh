#!/bin/sh

xset -b

xhost +SI:localuser:$USER

wmname LG3D

export _JAVA_AWT_WM_NONREPARENTING=1

export QT_QPA_PLATFORMTHEME=qt5ct
export XMODIFIERS=@im=exim  
export GTK_IM_MODULE=xim    
export QT_IM_MODULE=xim     
export CLUTTER_IM_MODULE=xim
export LSP_USE_PLISTS=true

setxkbmap -option 'caps:ctrl_modifier,grp:shifts_toggle,grp_led:caps'

xsetroot -cursor_name left_ptr

export VISUAL=emacsclient
export EDITOR="$VISUAL"

/usr/local/bin/emacs --daemon

exec dbus-launch --exit-with-session emacs

# #!/bin/sh

# xset -b

# xhost +SI:localuser:$USER

# wmname LG3D

# export _JAVA_AWT_WM_NONREPARENTING=1

# export QT_QPA_PLATFORMTHEME=qt5ct
# export XMODIFIERS=@im=exim  
# export GTK_IM_MODULE=xim    
# export QT_IM_MODULE=xim     
# export CLUTTER_IM_MODULE=xim
# export LSP_USE_PLISTS=true

# setxkbmap -option 'caps:ctrl_modifier,grp:shifts_toggle,grp_led:caps'

# xsetroot -cursor_name left_ptr

# export VISUAL=emacsclient
# export EDITOR="$VISUAL"

# #/usr/local/bin/emacs --daemon

# eval $(dbus-launch --sh-syntax)

# dbus-update-activation-environment --all

# export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh)"
# export GPG_TTY=$(tty)

# eval $(gnome-keyring-daemon --start --components=pkcs11,secrets,ssh,gpg)

# exec dbus-launch --exit-with-session emacs
# 
