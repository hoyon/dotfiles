#!/bin/sh

res=$(pgrep -c wofi)

if [ $res -eq 0 ]; then
    exec wofi --show drun --insensitive --location top --lines 10 --no-actions
fi

