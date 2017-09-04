#
# zsh/tmux.zsh
#


# Integrate `tmux` attach with `percol`
# Credit: https://github.com/mooz/percol
if exists percol; then
    function tmattach() {
        if [[ $1 == "" ]]; then
            PERCOL=percol
        else
            PERCOL="percol --query $1"
        fi

        sessions=$(tmux list-sessions)
        [ $? -ne 0 ] && return

        session=$(echo ${sessions} | eval ${PERCOL} | cut -d: -f1)
        if [[ -n "${session}" ]]; then
            tmux attach -t ${session}
        fi
    }
fi


# Handy aliases
if exists tmattach; then
    alias tma=tmattach
else
    alias tma='tmux attach -t'
fi
alias tml='tmux list-sessions'
alias tms='tmux new-session -s'


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
