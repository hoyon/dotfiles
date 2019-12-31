#!/usr/bin/bash

ENGINE=$(ibus engine)
JP=mozc-jp
GB=xkb:gb:extd:eng

if [ "$ENGINE" = $JP ]; then
    notify-send -t 1000 English
    ibus engine $GB
else
    notify-send -t 1000 Japanese
    ibus engine $JP
fi
