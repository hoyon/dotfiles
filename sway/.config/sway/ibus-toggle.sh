#!/usr/bin/bash

ENGINE=$(ibus engine)
JP=anthy
GB=xkb:gb::eng

if [ -z "$ENGINE" ]; then
    notify-send -t 1000 "ibus not running"
    exit 1
fi

if [ "$ENGINE" = $JP ]; then
    notify-send -t 1000 English
    ibus engine $GB
else
    notify-send -t 1000 Japanese
    ibus engine $JP
fi
