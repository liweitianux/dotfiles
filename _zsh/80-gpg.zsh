#
# zsh/gpg.zsh
#

# NOTE:
# ZSH is configured to launch `gpg-agent' on login, so it may be necessary
# to disable its launch by systemd, e.g.,
#     $ systemctl --global --user mask --now \
#           gpg-agent.service gpg-agent.socket gpg-agent-ssh.socket \
#           gpg-agent-extra.socket gpg-agent-browser.socket

# This `GPG_TTY' variable should be set to the correct TTY where the shell
# is running.  See `gpg-agent(1)' for more details.
export GPG_TTY=$(tty)

# Set SSH to use `gpg-agent' as the SSH agent support is enabled
# See `gpg-agent(1)' for more details.
unset SSH_AGENT_PID
if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
fi

# Restart `gpg-agent'
restart-gpg-agent() {
    local pinentry_arg
    [ -x "${HOME}/bin/pinentry" ] && \
        pinentry_arg="--pinentry-program ${HOME}/bin/pinentry" || \
        pinentry_arg=""
    gpgconf --kill gpg-agent >/dev/null
    gpg-agent --daemon --enable-ssh-support ${pinentry_arg} >/dev/null
}

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
#
update-gpg-tty() {
    gpg-connect-agent updatestartuptty /bye >/dev/null 2>&1
}

update-pinentry-app() {
    echo "term" > ${XDG_RUNTIME_DIR}/pinentry-app
}

# Hook the above functions to be exec'ed before every command execuation
#
# Credit: http://blog.mrloop.com/workflow/2017/02/09/pin-entry.html
#
autoload -U add-zsh-hook
zsh-preexec() {
    update-gpg-tty && update-pinentry-app || true
}
add-zsh-hook preexec zsh-preexec


# Delete all identities from the `gpg-agent', which is similar to
# `ssh-add -D`.
#
# Credit: http://blog.mrloop.com/workflow/2017/02/09/pin-entry.html
#
ssh-delete() {
    grep -o '^[A-Z0-9]*' ${HOME}/.gnupg/sshcontrol | \
        xargs -I'%' rm ${HOME}/.gnupg/private-keys-v1.d/'%'.key
    echo "" > ${HOME}/.gnupg/sshcontrol
}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
