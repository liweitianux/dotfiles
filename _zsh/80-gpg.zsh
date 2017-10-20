#
# zsh/gpg.zsh
#

# NOTE: `gpg-agent' is configured to be launched in `~/.profile'.

# This `GPG_TTY' variable should be set to the correct TTY where the shell
# is running.  See `gpg-agent(1)' for more details.
export GPG_TTY=$(tty)

# Set SSH to use `gpg-agent' as the SSH agent support is enabled
# See `gpg-agent(1)' for more details.
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# Since the 'ssh-agent' protocol does not contain a mechanism for telling
# the agent on which terminal/display it is running, gpg-agent's
# ssh-support can just use the TTY or X display when `gpg-agent' has been
# started, which may be before the X session startup.  Therefore, when the
# switched to the X session, or login remotely through SSH, the `pinentry'
# will get popped up on whatever display the `gpg-agent' has been started
# or may just fail.  In this case, a manual update is necessary.
#
# This will set startup TTY and X11 DISPLAY variables to the values of
# this session.
#
# Credits:
# * GnuPG: Commonly Seen Problems
#   https://www.gnupg.org/documentation/manuals/gnupg/Common-Problems.html
# * `gpg-agent(1)': option `--enable-ssh-support'
#
gpg-connect-agent updatestartuptty /bye >/dev/null


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
