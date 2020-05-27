#!/usr/bin/sh

TASK=$(wofi --dmenu --exec-search --cache-file /dev/null --prompt "" --lines 1 --location top)

TRIMMED=$(echo "$TASK" | xargs)

if [ -n "$TRIMMED" ]; then
    todoist-cli quick "$TRIMMED"
fi
