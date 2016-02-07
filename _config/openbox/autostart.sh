#!/bin/sh
#
# ~/.config/openbox/autostart.sh
#
# Aaron LI
# 2016-01-31
#

# disable X beep
xset b off

# set wallpaper
nitrogen --restore &

pgrep -x redshift     || redshift &
pgrep -x urxvtd       || urxvtd -q -f -o &
pgrep -x fcitx        || fcitx &
pgrep -x tint2        || tint2 &
pgrep -x xcompmgr     || xcompmgr &
pgrep -x xscreensaver || xscreensaver -no-splash &
pgrep -x mpd          || mpd &

# pgrep -x parcellite || parcellite &
#xfce4-power-manager &

