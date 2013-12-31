#!/bin/bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Bind Caps Lock to Control
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# Inform the X server of new font directories and force a rescan
xset +fp /usr/share/fonts/local
xset +fp /home/edward/.fonts
xset fp rehash

slim=$1
session=$1

# This script may have been called by slim like this:
#  $ exec /bin/bash -login ~/.xinitrc %session
[[ -z $slim ]] && return

case $session in
    dwm)
        # Set a background image
        feh --bg-scale /usr/share/slim/themes/default/background.jpg &

        # Launch statusbar script
        ~/.dwm-statusbar.sh &

        # Fix Fn volume controls
        xbindkeys

        exec dwm
        ;;

    xfce4)
        exec startxfce4
        ;;

    *)
        logger "$0: invoked with invalid desktop evironment: $desktop_env"
        ;;
esac
