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
if [ -d "$HOME/bin" ]; then
    export PATH="$HOME/bin:$PATH"
fi
# admin
if `groups | grep -qE '\b(wheel|adm|sudo)\b'`; then
    export PATH="$PATH:/usr/local/sbin:/usr/sbin:/sbin"
fi
# TeXlive
export PATH=$PATH:/usr/local/texlive/bin/x86_64-linux

###### gpg agent ######
gpgenv="$HOME/.gnupg/gpg-agent.env"
if [ -e "${gpgenv}" ] && kill -0 $(grep GPG_AGENT_INFO "${gpgenv}" \
        | cut -d: -f 2) 2>/dev/null; then
    eval "$(cat "${gpgenv}")"
else
    eval "$(gpg-agent --daemon --enable-ssh-support --write-env-file "${gpgenv}")"
fi
export GPG_AGENT_INFO
export SSH_AUTH_SOCK    # enable gpg-agent for ssh

# This file is sourced by bash for login shells.  The following line
# runs your .bashrc and is recommended by the bash info pages.
[ -f ~/.bashrc ] && . ~/.bashrc

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=sh: #
