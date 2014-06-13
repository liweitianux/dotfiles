#!/bin/sh
#
# exit script used in i3 wm
#

PNG_IMAGE="~/.i3/images/wallpaper.png"

lock() {
    if [ -f ${PNG_IMAGE} ]; then
        i3lock -d -ti "${PNG_IMAGE}"
    else
        i3lock -d -c 000000
    fi
}

case "$1" in
    lock)
        lock
        ;;
    logout)
        i3-msg exit
        ;;
    suspend)
        lock
        dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Suspend
        ;;
    hibernate)
        lock
        dbus-send --system --print-reply --dest="org.freedesktop.UPower" /org/freedesktop/UPower org.freedesktop.UPower.Hibernate
        ;;
    reboot)
        dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Restart
        ;;
    shutdown)
        dbus-send --system --print-reply --dest="org.freedesktop.ConsoleKit" /org/freedesktop/ConsoleKit/Manager org.freedesktop.ConsoleKit.Manager.Stop
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
        exit 2
esac

exit 0

