#
# zsh/gpg.zsh
#

# NOTE:
# Install both the `pinentry-gtk-2' and `pinentry-curses', and symlink
# `pinentry-gtk-2' to `pinentry' as the default pinentry program, which
# will fallback to the text mode when X11 is not avaiable (i.e.,
# `$DISPLAY' is not set), e.g., through SSH logins.
# `pinentry-gnome3' seems to have problem that cannot fallback to the
# text mode ... (for reason unkown ...)

# This `GPG_TTY' variable should be set to the correct TTY where the shell
# is running.  See `gpg-agent(1)' for more details.
export GPG_TTY=$(tty)

# Make SSH to use `gpg-agent'.
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# Use curses-based pinentry for SSH logins
# Credit: https://wiki.gentoo.org/wiki/GnuPG
if [ -n "${SSH_CONNECTION}" ] ;then
    export PINENTRY_USER_DATA="USE_CURSES=1"
fi

# Let pinentry know which console to display in for `ssh-agent'.
#
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
# * http://blog.mrloop.com/workflow/2017/02/09/pin-entry.html
#
update-gpg-tty() {
    gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1 || true
}
autoload -U add-zsh-hook
add-zsh-hook preexec update-gpg-tty

# Delete all identities from the `gpg-agent', which is similar to
# `ssh-add -D`.
# Credit: http://blog.mrloop.com/workflow/2017/02/09/pin-entry.html
ssh-delete() {
    grep -o '^[A-Z0-9]*' ${HOME}/.gnupg/sshcontrol | \
        xargs -I'%' rm ${HOME}/.gnupg/private-keys-v1.d/'%'.key
    echo "" > ${HOME}/.gnupg/sshcontrol
}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
