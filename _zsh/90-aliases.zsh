#
# zsh/aliases.zsh
#

alias zhelp='run-help'

alias ..='cd ../'
alias ...='cd ../../'
alias ....='cd ../../../'
alias cd..='cd ..'
alias d='dirs -v | head -10'
alias po=popd
alias pu=pushd

if [[ -n "${IS_LINUX}" ]]; then
    alias ls='ls --color=auto'
elif [[ -n "${IS_BSD}" ]] || [[ -n "${IS_MAC}" ]]; then
    alias ls='ls -G'
fi
alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias lsa='ls -lah'

# Do not use `GREP_OPTIONS`
alias grep='grep --color=auto'

exists "vi" || alias vi=vim
exists "safe-rm" && alias rm=safe-rm

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
