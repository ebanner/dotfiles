[[ -f ~/.Xresources ]] && xrdb -merge ~/.Xresources

# Set a background image
feh --bg-scale /usr/share/slim/themes/default/background.jpg &

# Launch statusbar script
$HOME/.dwm-statusbar.sh &

# Inform the X server of new directories and force a rescan
xset +fp /usr/share/fonts/local
xset +fp /home/edward/.fonts
xset fp rehash

# Fn volume controls
xbindkeys

# Set keyboard layout to dvorak
setxkbmap dvorak

exec dbus-launch dwm
#exec ck-launch-session dbus-launch dwm
#exec gnome-session
#exec startlxde
