#!/bin/bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set layout to colemak
setxkbmap us -variant colemak #&& [[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

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

    exec dwm
    ;;

  xfce4)
    # Fall back to xfce
    exec startxfce4
    ;;

  *)
    # Do nothing
    ;;
esac
