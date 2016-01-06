#!/bin/sh
#
# Compose mail in new tmux/screen window.
#
# Reference:
# [1] Multi-window Mutt with Screen
#     http://op-co.de/blog/posts/mulit-window_mutt/
#

# set the screen window title to the message receiver
#awk -F 'To: ' '/^To:/ { print "\033k" $2 "\033\\" }' "$1"

mutt -F ~/.mutt/compose.rc -H "$1"
rm "$1"
