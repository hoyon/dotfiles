#!/bin/bash

bluetoothctl power on && bluetoothctl connect 6C:AB:31:F3:9C:5D

sleep 1



if bluetoothctl info 6C:AB:31:F3:9C:5D | grep "Connected: yes"; then
    notify-send -t 1000 "Connected"
else
    notify-send -t 1000 "Not connected"
fi
