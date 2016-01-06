# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

###### locale ######
export LANG="en_US.UTF-8"
export LC_CTYPE="zh_CN.UTF-8"
export LC_COLLATE="C"

###### input method ######
export XMODIFIERS="@im=fcitx"
export INPUT_METHOD="fcitx"
export GTK_IM_MODULE="fcitx"
export GTK3_IM_MODULE="fcitx"
export QT_IM_MODULE="fcitx"
export QT4_IM_MODULE="fcitx"


###### PATH ######
# admin
if groups | grep -qE '\b(wheel|adm|sudo)\b'; then
    export PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
fi
# $HOME/bin
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi
# npm
if [ -d "$HOME/npm/bin" ]; then
    export PATH="$PATH:$HOME/npm/bin"
fi


###### Xmodmap ######
[ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap


###### ConTeXt ######
export OSFONTDIR="/usr/local/share/fonts;$HOME/.fonts"


###### bash ######
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=sh: #
