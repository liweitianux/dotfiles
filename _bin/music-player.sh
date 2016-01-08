#!/bin/sh
#
# Music player control wrapper to support different players.
#
# Aaron LI
# Created: 2016-01-07
# Updated: 2016-01-08
#

PLAYERS="mpd mocp"
#PLAYERS="mocp mpd"


if [ $# -ne 1 ]; then
    echo "Usage:"
    echo "    `basename $0` < state | info | play | pause | toggle | stop | previous | next >"
    exit 1
fi


escape() {
    echo "$1" | sed 's/&/\&amp;/g; s/</\&lt;/g; s/>/\&gt;/g;'
}


notify() {
    title="$1"
    text=`escape "$2"`
    icon="$3"
    notify-send -t 5000 --hint=int:transient:1 -i "${icon}" "${title}" "${text}"
}

# mpd/mpc
player_mpd() {
    case "$1" in
        state)
            status=`mpc status`
            if echo "${status}" | grep -q '^\[playing\]'; then
                echo "playing"
            elif echo "${status}" | grep -q '^\[paused\]'; then
                echo "paused"
            else
                echo "stopped"
            fi
            ;;
        info)
            state=`player_mpd state`
            #echo "state: ${state}"
            if [ "${state}" = "stopped" ]; then
                notify "MPD" "stopped"
            else
                artist=`mpc current -f "%artist%"`
                album=`mpc current -f "[%date% - ]%album%"`
                title=`mpc current -f "%title%"`
                notify "${title}" "${artist}\n${album}"
            fi
            ;;
        play)
            mpc play
            ;;
        pause)
            mpc pause
            ;;
        toggle)
            mpc toggle
            ;;
        stop)
            mpc stop
            ;;
        previous)
            mpc prev
            ;;
        next)
            mpc next
            ;;
        *)
            echo "ERROR: mpd/mpc: unknown command!"
            exit 11
            ;;
    esac
}


# moc/mocp
player_mocp() {
    case "$1" in
        state)
            state=`mocp -Q "%state"`
            [ "${state}" = "PLAY"  ] && echo "playing"
            [ "${state}" = "STOP"  ] && echo "stopped"
            [ "${state}" = "PAUSE" ] && echo "paused"
            ;;
        info)
            state=`mocp -Q "%state"`
            if [ "${state}" = "STOP" ]; then
                notify "MOC" "stopped"
            else
                artist=`mocp -Q "%artist"`
                album=`mocp -Q "%album"`
                title=`mocp -Q "%song"`
                notify "${title}" "${artist}\n${album}"
            fi
            ;;
        play)
            state=`mocp -Q "%state"`
            [ "${state}" = "STOP" ] && mocp -p || mocp -U
            ;;
        pause)
            mocp -P
            ;;
        toggle)
            mocp -G
            ;;
        stop)
            mocp -s
            ;;
        previous)
            mocp -r
            ;;
        next)
            mocp -f
            ;;
        *)
            echo "ERROR: mocp: unknown command!"
            exit 21
            ;;
    esac
}


for player in ${PLAYERS}; do
    if pgrep -x ${player} >/dev/null 2>&1; then
        player_cmd="player_${player}"
        #echo "Found player: ${player}"
        break
    fi
done

if [ -n "${player_cmd}" ]; then
    eval ${player_cmd} "$1"
else
    echo "ERROR: no known music player running/found"
    exit 31
fi

