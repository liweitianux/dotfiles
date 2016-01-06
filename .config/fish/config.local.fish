#
# ~/.config/fish/config.local.fish
#
# Local configuration for fish.
#
# Aaron LI
# 2015-10-03
#


## astro {{{
# backup LD_LIBRARY_PATH settings
set -x LD_LIBRARY_PATH_BAK $LD_LIBRARY_PATH

# heasoft
set -x HEADAS "$HOME/local/heasoft/heasoft-6.16/x86_64-unknown-linux-gnu"
function heainit
    set -l hea_state (echo $PATH | tr ':' '\n' | grep 'heasoft')
    if test "x$hea_state" = "x"
        source $HEADAS/headas-init.fish
    end
    set -x LD_LIBRARY_PATH $LD_LIBRARY_PATH_BAK
end

# ciao
set -x CIAO_PATH "$HOME/local/ciao/ciao-4.6"
function ciaoinit
    heainit
    source "$CIAO_PATH/bin/ciao.fish" $argv
    set -x CIAO_LD_LIBRARY_PATH "$ASCDS_INSTALL/ots/lib"
end
## astro }}}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=fish: #
