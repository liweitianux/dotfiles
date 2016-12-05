#
# zsh/exports.zsh
#

# NOTE:
# Generic environment variables and those needing been set only once
# should go to `~/.profile'.

# GnuPG: see 'gpg-agent(1)'
export GPG_TTY=$(tty)
# Refresh 'gpg-agent' tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
