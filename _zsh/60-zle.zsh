#
# zsh/zle.zsh
# some generic ZLE settings
# see zshzle(1)
#
# 2016-05-29
#

# Turn off ZLE bracketed paste in dumb term,
# otherwise turn on ZLE bracketed-paste-magic
# Credit: http://zmwangx.github.io/blog/2015-09-21-zsh-51-and-bracketed-paste.html
# See also zshparam(1)
if [[ ${TERM} == dumb ]]; then
    unset zle_bracketed_paste
else
    autoload -Uz bracketed-paste-magic
    zle -N bracketed-paste bracketed-paste-magic
fi

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
