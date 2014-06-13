# ~/.bashrc
# /etc/skel/.bashrc
#
# This file is sourced by all *interactive* bash shells on startup,
# including some apparently interactive shells such as scp and rcp
# that can't tolerate any output.  So make sure this doesn't display
# anything or bad things will happen !

# Test for an interactive shell.  There is no need to set anything
# past this point for scp and rcp, and it's important to refrain from
# outputting anything in those cases.
if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi


### bash settings {{{
# no double entries in the shell history
export HISTCONTROL="$HISTCONTROL erasedups:ignoreboth"

# prompt
export PS1="\[\033[01;33m\]\u@\h\[\033[01;34m\] \w \$\[\033[00m\] "
### bash settings }}}


### bash completion {{{
# bash-completion
if [ -f /etc/profile.d/bash-completion.sh ]; then
    . /etc/profile.d/bash-completion.sh
fi
### bash completion }}}


### environment variables {{{
export EDITOR=vim

### }}}


### aliases {{{
alias ll='ls -l'
alias lf='ls -F'
alias la='ls -A'

# ~/.bash_aliases
[ -f ~/.bash_aliases ] && . ~/.bash_aliases
### aliases }}}


### run applications {{{
# music player daemon (mpd)
[ ! -s ~/.mpd/pid ] && mpd >/dev/null 2>&1
### run applications }}}

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=sh: #
