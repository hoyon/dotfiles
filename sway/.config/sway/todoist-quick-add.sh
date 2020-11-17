#!/usr/bin/sh

TASK=$(wofi --dmenu --exec-search --cache-file /dev/null --lines 1 --location top --hide-scroll)

TRIMMED=$(echo "$TASK" | xargs)

if [ -n "$TRIMMED" ]; then
    todoist quick "$TRIMMED"
fi
