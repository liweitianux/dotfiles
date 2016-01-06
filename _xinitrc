#!/bin/sh
#
# ~/.xinitrc
#

# Run all system xinitrc shell scripts.
xinitdir="/etc/X11/xinit/xinitrc.d"
if [ -d "$xinitdir" ]; then
    for script in $xinitdir/*; do
        echo "Loading xinit script $script"
        if [ -x "$script" -a ! -d "$script" ]; then
            . "$script"
        fi
    done
    unset $script
fi

[ -f ~/.Xresources ] && xrdb -load ~/.Xresources

# disable beep
xset b off

# disable CapsLock
setxkbmap -option ctrl:nocaps

