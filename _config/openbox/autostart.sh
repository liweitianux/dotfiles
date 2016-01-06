#!/bin/sh
feh --bg-fill '/home/ly/pictures/wallpapers/.current' &
#nitrogen --restore &
#ibus-daemon -d -x
pgrep fcitx || fcitx &
pkill tint2
tint2 &
#xcompmgr -CfF &
pgrep xcompmgr || xcompmgr &
pgrep xscreensaver || xscreensaver -no-splash &
#wicd-client &
#scrolling.sh &
#xfce4-volumed
#xmms2-launcher
#mpd &
#parcellite &
pgrep urxvtd || urxvtd -q -f -o &
#/usr/bin/conky -q &
#xfce4-power-manager &
