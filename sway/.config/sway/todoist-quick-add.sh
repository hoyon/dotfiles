#!/usr/bin/sh

TASK=$(wofi --dmenu --exec-search --cache-file /dev/null --prompt "Quick add task" --height 1)

TRIMMED=$(echo "$TASK" | xargs)

if [ -n "$TRIMMED" ]; then
    todoist-cli quick "$TRIMMED"
fi
