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

#alias history='fc -l 1'

alias ls='ls --color=auto'
alias l='ls -lah'
alias la='ls -lAh'
alias ll='ls -lh'
alias lsa='ls -lah'

alias grep='grep --color=auto'

alias e='emacs'
alias v='vim'

alias ta='tmux attach -t'
alias tl='tmux list-sessions'
alias ts='tmux new-session -s'
alias tkss='tmux kill-session -t'
alias tksv='tmux kill-server'

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
