#
# zsh/term.zsh
#
# 2018-02-02
#

# Disable sending of start (Ctrl-Q) and stop (Ctrl-S) characters
stty -ixoff

# Disable XON/XOFF flow control
stty -ixon

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
