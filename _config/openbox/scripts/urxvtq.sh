#!/bin/sh
#
# Make a urxvt client behave like Quake with `xdotool`.
#
# Credits:
# [1] ArchWiki - rxvt-unicode - Improved Quake-like behavior in Openbox
#     https://wiki.archlinux.org/index.php/rxvt-unicode#Improved_Kuake-like_behavior_in_Openbox
# [2] HOWTO: get a quake-like urxvt terminal in openbox
#     https://bbs.archlinux.org/viewtopic.php?pid=550380
#
# Aaron LI
# Created: 2015-01-09
# Updated: 2015-01-09
#

this=$(realpath $0)
this_dir=$(dirname $this)

wid=$(xdotool search --classname "^urxvtq$")

if [ -z "$wid" ]; then
    $this_dir/urxvtc.sh -name urxvtq -geometry 90x24
    wid=$(xdotool search --classname "^urxvtq$")
    xdotool windowfocus "$wid"
    xdotool key Control_L+l
elif [ -z "$(xdotool search --onlyvisible --classname '^urxvtq$' 2>/dev/null)" ]; then
    xdotool windowmap   "$wid"
    xdotool windowfocus "$wid"
else
    xdotool windowunmap "$wid"
fi

