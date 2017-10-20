#
# zsh/exports.zsh
#

# NOTE:
# Generic environment variables and those needing been set only once
# should go to `~/.profile'.

# colors for BSD ls
if [[ -n "${IS_BSD}" ]]; then
    export CLICOLOR=1
    export LSCOLORS=exfxcxdxbxegedabagacad
fi

# Color setup for `ls': `LS_COLORS'
# NOTE: For unknown reason, the `LS_COLORS' variable get overridden when
#       it is set in `~/.profile'.
if exists dircolors; then
    eval $(dircolors -b)
fi


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
