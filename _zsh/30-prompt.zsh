#
# zsh/prompt.zsh
#
# Credit: http://chneukirchen.org/dotfiles/.zshrc
#


# gitpwd - print %~, limited to $NDIR segments, with inline git branch
NDIRS=3
function gitpwd() {
    local -a segs splitprefix
    local prefix branch
    segs=("${(Oas:/:)${(D)PWD}}")
    segs=("${(@)segs/(#b)(?(#c10))??*(?(#c5))/${(j:\u2026:)match}}")

    if gitprefix=$(git rev-parse --show-prefix 2>/dev/null); then
        splitprefix=("${(s:/:)gitprefix}")
        if ! branch=$(git symbolic-ref -q --short HEAD); then
           branch=$(git name-rev --name-only HEAD 2>/dev/null)
           [[ $branch = *\~* ]] || branch+="~0"    # distinguish detached HEAD
        fi
        if (( $#splitprefix > NDIRS )); then
           print -n "${segs[$#splitprefix]}@$branch "
        else
           segs[$#splitprefix]+=@$branch
        fi
    fi

    (( $#segs == NDIRS+1 )) && [[ $segs[-1] == "" ]] && print -n /
    print "${(j:/:)${(@Oa)segs[1,NDIRS]}}"
}


function myprompt() {
    case "$TERM" in
    xterm*|rxvt*)
        precmd()  { [[ -t 1 ]] && print -Pn "\e]0;%m: %~\a" }
        preexec() { [[ -t 1 ]] && print -n "\e]0;$HOST: ${(q)1//(#m)[$'\000-\037\177-']/${(q)MATCH}}\a" }
    esac
    setopt PROMPT_SUBST
    nbsp=$'\u00A0'
    PROMPT='%B%F{green}%m%(?.. %F{red}%??)%(1j. %F{yellow}%j&.)%b%f $(gitpwd)%B%(!.%F{red}.%F{green})%#${SSH_CONNECTION:+%#}$nbsp%b%f'
    RPROMPT=''
    # Prompt for spelling correction
    SPROMPT='zsh: correct %B%F{red}%R%b%f to %B%F{green}%r%b%f [(y)es (n)o (a)bort (e)dit]? '
}


# Setup prompts
myprompt


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
