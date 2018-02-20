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

# Delete all identities from the `gpg-agent', which is similar to
# `ssh-add -D`.
# Credit: http://blog.mrloop.com/workflow/2017/02/09/pin-entry.html
ssh-delete() {
    grep -o '^[A-Z0-9]*' ${HOME}/.gnupg/sshcontrol | \
        xargs -I'%' rm ${HOME}/.gnupg/private-keys-v1.d/'%'.key
    echo "" > ${HOME}/.gnupg/sshcontrol
}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
