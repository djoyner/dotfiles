#!/bin/sh

# If HHKB Professional 2 keyboard is installed, map eject key
if lsusb -d 0853:0100 > /dev/null;
then
    xmodmap -e "keycode 198 = XF86Eject"
fi
