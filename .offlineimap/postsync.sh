#!/bin/sh
#
# OfflineIMAP postsynchook
#
# Weitian LI
# 2015/02/02
#

## Check new mails & send notification
MAILDIR="${HOME}/Mail/"
newmails=0
for d in `find ${MAILDIR} -maxdepth 2 -type d -iname '*inbox'`; do
    n=`ls ${d}/new/ | wc -l`
    newmails=`expr ${newmails} + ${n}`
done

if [ ${newmails} -gt 0 ] && which notify-send >/dev/null 2>&1; then
    export DISPLAY=":0"
    export XAUTHORITY="${HOME}/.Xauthority"
    notify-send -i 'mail-unread' -a "OfflineIMAP" \
        "OfflineIMAP: Received ${newmails} new mail(s)!"
fi

## Invoke notmuch to index mails
notmuch new

