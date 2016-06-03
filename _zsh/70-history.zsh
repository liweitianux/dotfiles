#
# zsh/history.zsh
#

HISTFILE=~/.zsh_history

HISTSIZE=10000
SAVEHIST=9000
# ignore these commands without arguments
HISTIGNORE="cd:ls:ll"


# Combine history search with `percol`
# Credit: https://github.com/mooz/percol
if exists percol; then
    function percol_select_history() {
        local tac
        exists gtac && tac="gtac" || {
                exists tac && tac="tac" || {
                        tac="tail -r" } }
        BUFFER=$(fc -l -n 1 | eval ${tac} | percol --query "${LBUFFER}")
        CURSOR=${#BUFFER}  # move cursor
        zle -R -c  # refresh
    }

    zle -N percol_select_history
    # Override the bindkey settings in `60-bindkeys.zsh`
    bindkey '^R' percol_select_history
fi

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
