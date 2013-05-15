[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

## display conky output on the window pane up top
#conky | while read -r
#do 
#  xsetroot -name "$REPLY"
#done &

# set a pretty background image
feh --bg-scale /usr/share/slim/themes/default/background.jpg &

$HOME/.dwm-statusbar.sh &

# Inform the X server of new directories and force a rescan
xset +fp /usr/share/fonts/local
xset +fp /home/edward/.fonts
xset fp rehash

# Map Fn up and Fn down to handle the volume
xbindkeys

#exec ck-launch-session dbus-launch dwm
exec dbus-launch dwm

#exec gnome-session

#exec startlxde
