#!/bin/bash

[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Remap keyboard to dvorak and swap a few additional keys
setxkbmap dvorak && [[ -s ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

# Slim calls this script like this:
#   $ exec /bin/bash -login ~/.xinitrc %session
case $1 in
  xfce4)
    exec dbus-launch startxfce4
    ;;

  dwm)
    # Inform the X server of new font directories and force a rescan
    xset +fp /usr/share/fonts/local
    xset +fp /home/edward/.fonts
    xset fp rehash

    # Set a background image
    feh --bg-scale /usr/share/slim/themes/default/background.jpg &

    # Launch statusbar script
    $HOME/.dwm-statusbar.sh &

    # Fn volume controls
    xbindkeys

    exec dbus-launch dwm
    ;;

  *)
    # Fallback to xfce for now
    exec dbus-launch startxfce4
    ;;
esac
