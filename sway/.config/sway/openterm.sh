#!/bin/bash

FOCUSED=$(swaymsg -t get_tree | jq '.. | (.nodes? // empty)[] | select(.focused==true) | {app_id, pid}')

if [[ $FOCUSED =~ "kitty" ]]; then
    PID=$(echo "$FOCUSED" | jq .pid)
    CHILD_PID=$(pgrep -P "$PID")
    CWD=$(readlink "/proc/$CHILD_PID/cwd")
    exec kitty -d "$CWD"
else
    exec kitty
fi
