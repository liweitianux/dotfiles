#
# zsh/exports.zsh
#

# NOTE:
# Generic environment variables and those needing been set only once
# should go to `~/.profile'.

# This `GPG_TTY' variable should be set to the correct TTY where the shell
# is running.  See also `gpg-agent(1)'
export GPG_TTY=$(tty)

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
