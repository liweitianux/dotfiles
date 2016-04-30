#
# zsh/emacs.zsh
#
# Credits:
# https://github.com/robbyrussell/oh-my-zsh/tree/master/plugins/emacs
#
# 2016-02-21
#

function _emacsclient() {
    # Get list of available X windows.
    X=`emacsclient --alternate-editor '' --eval '(x-display-list)' 2>/dev/null`

    if [ -z "$X" ] || [ "$X" = "nil" ]; then
        # Create one if there is no X window yet.
        command emacsclient --alternate-editor "" --create-frame "$@"
    else
        # Prevent creating another X frame if there is at least one present.
        command emacsclient --alternate-editor "" "$@"
    fi
}

alias emacs='_emacsclient -t'
alias e=emacs
# Same as 'M-x eval' but outside of Emacs
alias eeval='_emacsclient --eval'
# Create a new X frame
alias eframe='_emacsclient --create-frame --no-wait'

# Write to stdout the path to the file opened in the current buffer
function efile() {
    local cmd="(buffer-file-name (window-buffer))"
    _emacsclient --eval "$cmd" | tr -d '"'
}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
