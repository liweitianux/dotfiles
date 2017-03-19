#
# zsh/hooks.zsh
#
# Credit: http://chneukirchen.org/dotfiles/.zshrc
#

case "$TERM" in
xterm*|rxvt*)
    function precmd()  {
        [[ -t 1 ]] && print -Pn "\e]0;%m: %~\a"
    }
    function preexec() {
        [[ -t 1 ]] && print -n "\e]0;$HOST: ${(q)1//(#m)[$'\000-\037\177-']/${(q)MATCH}}\a"
    }
    ;;
esac


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
