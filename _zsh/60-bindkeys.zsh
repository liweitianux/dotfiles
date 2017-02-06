#
# zsh/bindkeys.zsh
# see zshzle(1)
#
# To see the key combo you want to use just do:
# $ cat > /dev/null
# then press it.
#
# NOTE:
# Switching mode (e.g., `bindkey -v`) will *reset* the following settings!
#
# Credit:
# * oh-my-zsh: https://github.com/robbyrussell/oh-my-zsh
#   lib/key-bindings.zsh
#

# Make sure that the terminal is in application mode when zle is active,
# since only then values from $terminfo are valid
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init() {
        echoti smkx
    }
    function zle-line-finish() {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

# [Ctrl-r] - Search backward incrementally for a specified string.
# The string may begin with ^ to anchor the search to the beginning of the line.
bindkey '^r' history-incremental-search-backward

# [PageUp] - Up a line of history
if [[ "${terminfo[kpp]}" != "" ]]; then
    bindkey "${terminfo[kpp]}" up-line-or-history
fi
# [PageDown] - Down a line of history
if [[ "${terminfo[knp]}" != "" ]]; then
    bindkey "${terminfo[knp]}" down-line-or-history
fi

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
    autoload -U up-line-or-beginning-search
    zle -N up-line-or-beginning-search
    bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
    autoload -U down-line-or-beginning-search
    zle -N down-line-or-beginning-search
    bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

# [Home] - Go to beginning of line
if [[ "${terminfo[khome]}" != "" ]]; then
    bindkey "${terminfo[khome]}" beginning-of-line
fi
# [End] - Go to end of line
if [[ "${terminfo[kend]}" != "" ]]; then
    bindkey "${terminfo[kend]}"  end-of-line
fi

# [Space] - do history expansion
bindkey ' ' magic-space
# [Ctrl-RightArrow] - move forward one word
bindkey '^[[1;5C' forward-word
# [Ctrl-LeftArrow] - move backward one word
bindkey '^[[1;5D' backward-word

# [Shift-Tab] - move through the completion menu backwards
if [[ "${terminfo[kcbt]}" != "" ]]; then
    bindkey "${terminfo[kcbt]}" reverse-menu-complete
fi

# [Backspace] - delete backward
bindkey '^?' backward-delete-char
# [Delete] - delete forward
if [[ "${terminfo[kdch1]}" != "" ]]; then
    bindkey "${terminfo[kdch1]}" delete-char
else
    bindkey "^[[3~" delete-char
    bindkey "^[3;5~" delete-char
    bindkey "\e[3~" delete-char
fi

# Emacs style line editing
bindkey "^K"    kill-whole-line                      # ctrl-k
bindkey "^R"    history-incremental-search-backward  # ctrl-r
bindkey "^A"    beginning-of-line                    # ctrl-a
bindkey "^E"    end-of-line                          # ctrl-e
bindkey "[B"    history-search-forward               # down arrow
bindkey "[A"    history-search-backward              # up arrow
bindkey "^D"    delete-char                          # ctrl-d
bindkey "^F"    forward-char                         # ctrl-f
bindkey "^B"    backward-char                        # ctrl-b

# see zshcontrib(1)
autoload -U select-word-style
# bash-style word killing: word characters are alphanumeric characters only
select-word-style bash

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
