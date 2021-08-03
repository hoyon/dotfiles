#!/usr/bin/bash

enabled=$(fcitx5-remote)

if [ "$enabled" -eq 1 ]; then
    notify-send -t 1000 Japanese
    fcitx5-remote -o
else
    notify-send -t 1000 English
    fcitx5-remote -c
fi
