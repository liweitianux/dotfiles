#
# zsh/prompt.zsh
#

function git_prompt_info {
    local ref=$(=git symbolic-ref HEAD 2> /dev/null)
    local gitst="$(=git status 2> /dev/null)"

    if [[ -f .git/MERGE_HEAD ]]; then
        if [[ ${gitst} =~ "unmerged" ]]; then
        gitstatus=" %{$fg[red]%}unmerged%{$reset_color%}"
        else
        gitstatus=" %{$fg[green]%}merged%{$reset_color%}"
        fi
    elif [[ ${gitst} =~ "Changes to be committed" ]]; then
        gitstatus=" %{$fg[blue]%}!%{$reset_color%}"
    elif [[ ${gitst} =~ "use \"git add" ]]; then
        gitstatus=" %{$fg[red]%}!%{$reset_color%}"
    elif [[ -n `git checkout HEAD 2> /dev/null | grep ahead` ]]; then
        gitstatus=" %{$fg[yellow]%}*%{$reset_color%}"
    else
        gitstatus=''
    fi

    if [[ -n $ref ]]; then
        echo "%{$fg_bold[green]%}/${ref#refs/heads/}%{$reset_color%}$gitstatus"
    fi
}

PROMPT='%{$fg_bold[blue]%}%~%<< $(git_prompt_info)%{$fg_bold[red]%}%(?..[%?])${PR_BOLD_WHITE}>%{$reset_color%} '

SPROMPT="zsh: correct %{$fg_bold[red]%}%R%{$reset_color%} to %{$fg_bold[green]%}%r%{$reset_color%} [(y)es (n)o (a)bort (e)dit]? "

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
