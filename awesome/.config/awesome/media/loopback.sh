#!/bin/sh

pactl list modules | grep module-loopback > /dev/null
ret=$?

if [ $ret -eq 0 ]; then
    pactl unload-module module-loopback
    notify-send "Disabled loopback"
else
    pactl load-module module-loopback latency_msec=1
    notify-send "Enabled loopback"
fi
