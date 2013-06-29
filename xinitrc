#!/bin/bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set layout to dvorak
setxkbmap dvorak

# Slim calls this script like this:
#   $ exec /bin/bash -login ~/.xinitrc %session
case $1 in
  dwm)
    # Inform the X server of new font directories and force a rescan
    xset +fp /usr/share/fonts/local
    xset +fp /home/edward/.fonts
    xset fp rehash

    # Set a background image
    feh --bg-scale /usr/share/slim/themes/default/background.jpg &

    # Launch statusbar script
    ~/.dwm-statusbar.sh &

    # Fix Fn volume controls
    xbindkeys

    # Make all the modifications, but don't swap the number keys
    [[ -s ~/.Xmodmap.dwm ]] && xmodmap ~/.Xmodmap.dwm

    exec dwm
    ;;

  xfce4|*)
    # Make all the modifications, including swapping the number keys
    [[ -s ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

    # Fall back to xfce
    exec startxfce4
    ;;
esac
