#!/bin/sh
#
# Start urxvt client, and start urxvt server first if necessary.
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

urxvtc "$@"
if [ $? -eq 2 ]; then
   urxvtd -q -o -f
   urxvtc "$@"
fi

