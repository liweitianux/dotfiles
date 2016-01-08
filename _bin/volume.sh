#!/bin/sh
#
# Volume control wrapper to support different systems.
#
# Aaron LI
# Created: 2016-01-07
# Updated: 2016-01-07
#

# ALSA device/driver (Linux)
ALSA_DEV="Master"
# OSS device (FreeBSD/DragonFly)
OSS_DEV="vol"


if [ $# -ne 1 ]; then
    echo "Usage:"
    echo "    `basename $0` < +% | -% | toggle | mute | unmute >"
    exit 1
fi


# amixer: Linux ALSA
vol_amixer() {
    case "$1" in
        toggle)
            amixer set ${ALSA_DEV} toggle
            ;;
        mute)
            amixer set ${ALSA_DEV} mute
            ;;
        unmute)
            amixer set ${ALSA_DEV} unmute
            ;;
        +[1-9]*|-[1-9]*)
            sign=`echo "$1" | cut -c1`
            value="${1#?}"
            amixer set ${ALSA_DEV} "${value}%${sign}" unmute
            ;;
        *)
            echo "ERROR: amixer: unknown control command"
            exit 11
            ;;
    esac
}


# mixer: FreeBSD/DragonFly OSS
vol_mixer() {
    # FIXME: how to toggle/mute/unmute ???
    case "$1" in
        +[1-9]*|-[1-9]*)
            sign=`echo "$1" | cut -c1`
            value="${1#?}"
            mixer ${OSS_DEV} "${sign}${value}:${sign}${value}"
            ;;
        *)
            echo "ERROR: mixer: unknown control command"
            exit 21
            ;;
    esac
}


OS=`uname -s`
case "${OS}" in
    Linux)
        vol_amixer "$1"
        ;;
    FreeBSD|DragonFly)
        vol_mixer "$1"
        ;;
    *)
        echo "ERROR: currently unsupport operating system"
        exit 2
        ;;
esac

