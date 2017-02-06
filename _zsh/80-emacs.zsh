#
# zsh/emacs.zsh
#
# Credits:
# https://github.com/robbyrussell/oh-my-zsh/tree/master/plugins/emacs
#
# 2016-02-21
#

if exists emacsclient; then
    function eserver() {
        # NOTE:
        # Force "LC_CTYPE=zh_CN.UTF-8", otherwise, the Chinese input methods
        # (e.g., Fcitx) cannot be activated within Emacs (GUI) in non-Chinese
        # locale (e.g., "LANG=en_US.UTF-8" while "LC_CTYPE" unset)
        command env LC_CTYPE=zh_CN.UTF-8 emacs --daemon
    }

    function _emacsclient() {
        # Get list of available X windows.
        local X=$(emacsclient --alternate-editor '' \
                              --eval '(x-display-list)' 2>/dev/null)

        if [ -z "$X" ] || [ "$X" = "nil" ]; then
            # Create one if there is no X window yet.
            local create_frame="--create-frame"
        else
            local create_frame=""
        fi

        command env LC_CTYPE=zh_CN.UTF-8 \
            emacsclient --alternate-editor "" $create_frame "$@"
    }

    alias emacs='_emacsclient -t'
    # Same as 'M-x eval' but outside of Emacs
    alias eeval='_emacsclient --eval'
    # Create a new X frame
    alias eframe='_emacsclient --create-frame --no-wait'
fi  # END: exists emacsclient


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
