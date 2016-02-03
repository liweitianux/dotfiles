#
# zsh/hooks.zsh
#

# Put the string "hostname::/full/directory/path" in the title bar:
function set_term_title {
    echo -ne "\e]2;$PWD\a"
}

# Put the parentdir/currentdir in the tab
function set_term_tab {
    echo -ne "\e]1;$PWD:h:t/$PWD:t\a"
}

function precmd {
    set_term_title
    set_term_tab
}


#function set_running_app {
#    printf "\e]1; $PWD:t:$(history $HISTCMD | cut -b7- ) \a"
#}
#
#function preexec {
#    set_running_app
#}
#
#function postexec {
#    set_running_app
#}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
