#!/bin/sh

SEARCH=`rofi -dmenu -p "Zeal"`

if [ -n "$SEARCH" ]; then
    zeal "$SEARCH"
    wmctrl -x -a zeal
fi
