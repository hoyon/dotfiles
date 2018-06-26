#!/bin/fish

set query (rofi -dmenu -p "Zeal")

if test "$query"
    zeal "$query"
    wmctrl -x -a zeal
end
