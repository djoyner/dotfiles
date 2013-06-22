#!/bin/sh

# If HHKB Professional 2 keyboard is installed, map eject key
if lsusb -d 0853:0100 > /dev/null;
then
    xmodmap -e "keycode 198 = XF86Eject"
fi

# Clear out display manager background and normalize cursor
xsetroot -solid "#404040" -cursor_name left_ptr &

# Synchronize clipboard, cut buffer, etc.
autocutsel -fork
autocutsel -selection PRIMARY -fork

# Start xscreensaver
xscreensaver -no-splash &

# Run trayer
#trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 6 --transparent true --alpha 0 --tint 0x000000 --height 16 &  

# Run xmonad
exec xmonad
#exec gnome-session --session=xmonad-gnome-nopanel "$@"
