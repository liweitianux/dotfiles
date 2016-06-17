#
# zsh/exports.zsh
#

# set locale and PATH in '~/.profile'

export EDITOR='vim'
export PAGER='less'
export LESS='--ignore-case --raw-control-chars'

# Setup terminal, and turn on colors
#export TERM='xterm-256color'

# GnuPG: see 'gpg-agent(1)'
export GPG_TTY=$(tty)
# Refresh 'gpg-agent' tty in case user switches into an X session
gpg-connect-agent updatestartuptty /bye >/dev/null

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
