#
# zsh/colors.sh
#

autoload -U colors && colors

# The variables are wrapped in %{%}. This should be the case for every
# variable that does not contain space.
for COLOR in RED GREEN YELLOW BLUE MAGENTA CYAN BLACK WHITE; do
  eval PR_$COLOR='%{$fg_no_bold[${(L)COLOR}]%}'
  eval PR_BOLD_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done

eval RESET='$reset_color'
export PR_RED PR_GREEN PR_YELLOW PR_BLUE PR_WHITE PR_BLACK
export PR_BOLD_RED PR_BOLD_GREEN PR_BOLD_YELLOW PR_BOLD_BLUE
export PR_BOLD_WHITE PR_BOLD_BLACK

# colors for BSD ls
if [[ -n "${IS_BSD}" ]]; then
    export CLICOLOR=1
    #export LSCOLORS=Gxfxcxdxbxegedabagacad
    export LSCOLORS=exfxcxdxbxegedabagacad
fi


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
