#!/bin/sh

xset -b

#feh --bg-fill "$HOME/pictures/wallpapers/.current" &
nitrogen --restore &

pgrep -x urxvtd || urxvtd -q -f -o &
pgrep -x fcitx || fcitx &
pgrep -x tint2 || tint2 &
pgrep -x xcompmgr || xcompmgr &
pgrep -x xscreensaver || xscreensaver -no-splash &
pgrep -x mpd || mpd &

# pgrep -x parcellite || parcellite &
#xfce4-power-manager &

