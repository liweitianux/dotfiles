#!/bin/sh

CONKYRC="$HOME/.conky/cronograph/conkyrc"

sleep 5

conky -d -c ${CONKYRC} "$@"

exit 0

