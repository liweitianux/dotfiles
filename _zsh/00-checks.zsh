#
# zsh/checks.zsh
#
# Credits: http://zanshin.net/2013/02/02/zsh-configuration-from-the-ground-up/
#

if [[ $(uname) = 'Linux' ]]; then
    IS_LINUX=1
elif [[ $(uname) = 'Darwin' ]]; then
    IS_MAC=1
elif [[ $(uname) = 'FreeBSD' ]]; then
    IS_FREEBSD=1
    IS_BSD=1
elif [[ $(uname) = 'DragonFly' ]]; then
    IS_DRAGONFLY=1
    IS_BSD=1
fi

# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
