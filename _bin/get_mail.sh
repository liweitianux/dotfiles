#!/bin/sh

export PATH=$HOME/bin:$HOME/.bin:$HOME/.local/bin:$PATH

check-gpg-pass.sh && {
    mbsync -a
    notmuch new
}
