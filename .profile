###########################################################
# ~/.profile
#
###########################################################

###### locale ######
export LANG="en_US.utf8"
export LC_CTYPE="zh_CN.utf8"
export LC_COLLATE="C"

###### input method ######
export XMODIFIERS="@im=fcitx"
export INPUT_METHOD="fcitx"
export GTK_IM_MODULE="fcitx"
export GTK3_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"
export QT4_IM_MODULE="fcitx"

###### PATH ######
# $HOME/bin
export PATH=$HOME/bin:$PATH
# TeXlive
export PATH=$PATH:/usr/local/texlive/bin/x86_64-linux
# admin
export PATH=$PATH:/usr/local/sbin:/usr/sbin:/sbin

###### gpg agent ######
eval "$(gpg-agent --daemon)"

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=sh: #
