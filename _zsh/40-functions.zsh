#
# zsh/functions.zsh
#
# execute 'functions <func_name>' to show function details.
#

## Check the existence/accessibility of a command
# Credit: https://stackoverflow.com/a/677212/4856091
function exists() {
    # 'command' is POSIX-compliant and more portable;
    # 'hash' only searches for commands;
    # while 'type' also considers builtins and keywords.
    type "$1" >/dev/null 2>&1
}


## Check whether the program is running
function is_running() {
    pgrep -x -u "${USER}" "$1" /dev/null 2>&1
}


function zsh_recompile() {
    autoload -U zrecompile
    rm -f ~/.zsh/*.zwc
    [[ -f ~/.zshrc ]] && zrecompile -p ~/.zshrc
    [[ -f ~/.zshrc.zwc.old ]] && rm -f ~/.zshrc.zwc.old

    local f
    for f in ~/.zsh/**/*.zsh; do
        [[ -f $f ]] && zrecompile -p $f
        [[ -f $f.zwc.old ]] && rm -f $f.zwc.old
    done

    [[ -f ~/.zcompdump ]] && zrecompile -p ~/.zcompdump
    [[ -f ~/.zcompdump.zwc.old ]] && rm -f ~/.zcompdump.zwc.old

    source ~/.zshrc
}


function extract() {
  echo "Extracting '$1' ..."
  if [ -f "$1" ] ; then
      case "$1" in
          *.tar.bz2)
              tar xjf "$1";;
          *.tar.gz)
              tar xzf "$1";;
          *.bz2)
              bunzip2 "$1";;
          *.rar)
              unrar x "$1";;
          *.gz)
              gunzip "$1";;
          *.tar)
              tar xf "$1";;
          *.tbz2)
              tar xjf "$1";;
          *.tgz)
              tar xzf "$1";;
          *.zip)
              unzip "$1";;
          *.Z)
              uncompress "$1";;
          *.7z)
              7z x "$1";;
          *)
              echo "Unable to extract: '$1'" ;;
      esac
  else
      echo "Invalid file: '$1'"
  fi
}


function trash() {
    local path
    for path in "$@"; do
        # ignore any arguments
        if [[ "${path}" = -* ]]; then
            :
        else
        local dst="${path##*/}"
        # append the time if necessary
        while [ -e ~/.trash/"${dst}" ]; do
            dst="${dst} "$(date +%H-%M-%S)
        done
        command mv "${path}" ~/.trash/"${dst}"
        fi
    done
}


## Print a horizontal rule
function rule() {
    printf "%$(tput cols)s\n" | tr ' ' "${1:-=}"
}


## Interactive move/rename: making renaming long filenames less sucks
# Credit: http://chneukirchen.org/dotfiles/.zshrc
function imv() {
    local src dst
    for src; do
        [[ -e "$src" ]] || { print -u2 "$src: does not exist"; continue }
        dst="$src"
        vared dst
        [[ "$src" != "$dst" ]] && mkdir -p ${dst:h} && mv -n $src $dst
    done
}


## Print pre-defined C macros
# Credit: http://chneukirchen.org/dotfiles/.zshrc
ccdef() {
    ${1:-cc} $@[2,-1] -dM -E - </dev/null
}


## Run up to N CMD in parallel with ARGS
#    zapply [-jN] [-iv] CMD... -- ARGS...
#    CMD will be run as zsh command if it contains a $
#    without explicit '--', assume CMD is first argument
#    {} (or $1) may be used to access argument
# Credit: http://chneukirchen.org/dotfiles/.zshrc
zapply() {
    local s="$@[(i)--]" xopt=
    (( s > $# )) && argv[2]=(-- "$argv[2]") && s=2
    zparseopts -D -M -A xopt n: p t P: j:=P v=t i=p   # map to xargs(1) flags
    (( $@[(i){}] < s )) && xopt[-I]={}
    [[ $1 = *'$'* ]] && argv[1]=(zsh -c "$1" --) && (( s += 3 ))
    printf '%s\0' "$@[s+1,-1]" | xargs -0 -r -n1 ${(kv)=xopt} "$@[1,s-1]"
}


## Generate random password/string
randpass() {
    local len=${1:-16}
    tr -dc '[:alnum:]' </dev/urandom | head -c ${len} | xargs
}


# vim: set ts=8 sw=4 tw=0 fenc=utf-8 ft=zsh: #
