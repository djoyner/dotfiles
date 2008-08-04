xsetroot -cursor_name left_ptr -solid '#a0a0a0' &
~/.xmonad/tray.sh | dzen2 -bg 'black' -fg 'white' -h 16 -w 2560 -p -ta r -e 'onstart=lower' &
exec xmonad
