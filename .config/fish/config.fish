#
# ~/.config/fish/config.fish
#
#
# Aaron LI
# 2015-09-26
#

# vi mode
fish_vi_mode

set -U EDITOR vim

## PATH
# ~/bin
if test -d $HOME/bin
    set -U fish_user_paths $HOME/bin $fish_user_paths
end
# admin paths
if groups | grep -qE '\b(wheel|adm|sudo)\b'
    set -U fish_user_paths $fish_user_paths /usr/local/sbin /usr/sbin /sbin
end

# local config
set -l localconfig "$HOME/.config/fish/config.local.fish"
if test -f $localconfig
    source $localconfig
end

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=fish: #
